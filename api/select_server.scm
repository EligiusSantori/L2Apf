(module api racket/base
	(require
		racket/contract
		racket/tcp
		"../library/crypter.scm"
		"../library/structure.scm"
		"../library/network.scm"
		"../packet/login/client/select_server.scm"
		"../packet/login/server/play_fail.scm"
		"../packet/login/server/play_ok.scm"
		"../packet/game/client/protocol_version.scm"
		"../packet/game/server/key_packet.scm"
		"../packet/game/client/validate_auth.scm"
		"../packet/game/server/character_list.scm"
	)
	(provide select-server)

	(define (select-server connection server)
		(set-box-field! connection 'world server)
		(let () ; Взаимодействие с login-сервером
			(define input-port (get-box-field connection 'input-port))
			(define output-port (get-box-field connection 'output-port))
			(define crypter (get-box-field connection 'crypter))
			
			(send connection (login-client-packet/select-server (list
				(cons 'server-id (hash-ref server 'id))
				(cons 'login-key (get-box-field connection 'login-key))
			)))
			
			(let loop ()
				(let ((buffer (receive connection)))
					(case (get-packet-id buffer)
						((#x06) (let ((packet (login-server-packet/play-fail buffer)))
							(display "play fail: ")
							(displayln (cdr (assoc 'reason packet))) ; TODO message
							(disconnect connection)
						))
						((#x07) (let ((packet (login-server-packet/play-ok buffer)))
							(begin
								(set-box-field! connection 'game-key (get-field packet 'game-key))
								(set-box-field! connection 'session-id #f)
								(set-box-field! connection 'crypter #f)
								(set-box-field! connection 'rsa-key #f)
								(disconnect connection)
							)
						))
						(else #f)
					)
				)
			)
		)
		
		(if (get-box-field connection 'game-key) ; Взаимодействие с game-сервером
			(let ((host (hash-ref server 'address)) (port (hash-ref server 'port)))
				(let-values (((input-port output-port) (tcp-connect host port)))
					
					(set-box-field! connection 'input-port input-port)
					(set-box-field! connection 'output-port output-port)
				
					(send connection (game-client-packet/protocol-version (list
						(cons 'protocol (get-box-field connection 'protocol))
					)))
				
					(let loop ()
						(let ((buffer (receive connection)))
							(case (get-packet-id buffer)
								((#x00) (let ((packet (game-server-packet/key-packet buffer)))
									(begin
										(let ((crypter (make-crypter (get-field packet 'key))))
											(set-box-field! connection 'crypter crypter)
										)
										
										(send connection (game-client-packet/validate-auth (list
											(cons 'login (get-box-field connection 'account))
											(cons 'login-key (get-box-field connection 'login-key))
											(cons 'game-key (get-box-field connection 'game-key))
										)))
										(loop)
									)
								))
								((#x13) (let ((packet (game-server-packet/character-list buffer)))
									(define (transform i) (cons (cdr (assoc 'name i)) (box (cdr (assoc 'id i)))))
									
									(set-box-field! connection 'account #f)
									(set-box-field! connection 'protocol #f)
									(set-box-field! connection 'login-key #f)
									(set-box-field! connection 'game-key #f)
									
									(map transform (get-field packet 'list))
								))
								(else #f)
							)
						)
					)
				)
			)
			#f
		)
	)
)