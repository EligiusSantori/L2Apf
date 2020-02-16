(module logic racket/base
	(require
		racket/contract
		(relative-in "../."
			"system/structure.scm"
			"system/connection.scm"
			(only-in "packet/packet.scm" get-packet-id)
			"packet/login/client/login_auth.scm"
			"packet/login/server/login_fail.scm"
			"packet/login/server/login_ok.scm"
			"packet/login/client/server_list.scm"
			"packet/login/server/server_list.scm"
			"model/world.scm"
		)
	)
	(provide (contract-out
		(login (-> connection? string? string? (listof world?)))
	))

	(define (login cn login password)
		(begin
			(set-connection-account! cn login)
			(send-packet cn (login-client-packet/login-auth (list
				(cons 'login login)
				(cons 'password password)
				(cons 'session-id (connection-session-id cn))
				(cons 'rsa-key (connection-session-key cn))
			)))
			(set-connection-session-key! cn #f)

			(let loop ()
				(let ((buffer (read-packet cn)))
					(case (get-packet-id buffer)
						((#x01) (let ((packet (login-server-packet/login-fail buffer)))
							(disconnect connection)
							(raise-user-error "Authentication failed, reason:" (ref packet 'reason))
						))
						((#x03) (let ((packet (login-server-packet/login-ok buffer)))
							(set-connection-session-key! cn (ref packet 'login-key))
							(send-packet cn (login-client-packet/server-list (list
								(cons 'login-key (connection-session-key cn))
							)))
							(loop)
						))
						((#x04) (let ((packet (login-server-packet/server-list buffer)))
							(map make-world
								(filter (lambda (server) (ref server 'state))
								(ref packet 'list))
							)
						))
						(else #f)
					)
				)
			)
		)
	)
)
