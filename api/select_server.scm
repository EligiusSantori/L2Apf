(module logic racket/base
	(require
		racket/contract
		racket/tcp
		(relative-in "../."
			"library/extension.scm"
			"system/structure.scm"
			"system/crypter.scm"
			"system/connection.scm"
			(only-in "packet/packet.scm" get-packet-id)
			"packet/login/client/select_server.scm"
			"packet/login/server/play_fail.scm"
			"packet/login/server/play_ok.scm"
			"packet/game/client/protocol_version.scm"
			"packet/game/server/key_packet.scm"
			"packet/game/client/validate_auth.scm"
			"packet/game/server/character_list.scm"
			"model/protagonist.scm"
			"model/world.scm"
		)
	)
	(provide (contract-out
		(select-server (-> connection? world? (listof protagonist?)))
	))

	(define (select-server cn wr)
		(set-connection-world! cn wr)
		(let ((login-key (connection-session-key cn)) (host (world-server-host wr)) (port (world-server-port wr)))
			; Login-server interaction.
			(send-packet cn (login-client-packet/select-server (list
				(cons 'server-id (world-server-id wr))
				(cons 'login-key login-key)
			)))
			(let loop ()
				(let ((buffer (read-packet cn)))
					(case (get-packet-id buffer)
						((#x06) (let ((packet (login-server-packet/play-fail buffer)))
							(disconnect cn)
							(raise-user-error "Connection failed, reason:" (ref packet 'reason))
						))
						((#x07) (let ((packet (login-server-packet/play-ok buffer)))
							(set-connection-session-id! cn #f)
							(set-connection-session-key! cn (ref packet 'game-key))
							(disconnect cn)
						))
						(else (raise-user-error "Unexpected login server response, packet:" buffer))
					)
				)
			)

			; Game-server interaction.
			(let-values (((input-port output-port) (tcp-connect host port)))
				(write-buffer output-port (game-client-packet/protocol-version (list
					(cons 'protocol (connection-protocol cn))
				)))
				(let loop ()
					(let ((buffer (if (connection-session-id cn) (read-packet cn) (read-buffer input-port))))
						(case (get-packet-id buffer)
							((#x00) (let ((packet (game-server-packet/key-packet buffer)))
								(let ((crypter (make-crypter (ref packet 'key))) (pc (connection-packet-channel cn)))
									(set-connection-session-id! cn #t)
									(set-connection-read-thread! cn (thread (bind read-thread input-port crypter pc)))
									(set-connection-send-thread! cn (thread (bind send-thread output-port crypter)))
								)
								(send-packet cn (game-client-packet/validate-auth (list
									(cons 'login (connection-account cn))
									(cons 'login-key login-key)
									(cons 'game-key (connection-session-key cn))
								)))
								(loop)
							))
							((#x13) (let ((packet (game-server-packet/character-list buffer)))
								(map (lambda (data) (make-protagonist data)) (ref packet 'list))
							))
							(else (raise-user-error "Unexpected game server response, packet:" buffer))
						)
					)
				)
			)
		)
	)
)
