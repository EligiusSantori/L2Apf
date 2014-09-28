(module api racket/base
	(require
		racket/contract
		"../library/structure.scm"
		"../library/network.scm"
		"../packet/login/client/login_auth.scm"
		"../packet/login/server/login_fail.scm"
		"../packet/login/server/login_ok.scm"
		"../packet/login/client/server_list.scm"
		"../packet/login/server/server_list.scm"
	)
	(provide login)

	(define (login connection login password)
		(begin
			(set-box-field! connection 'account login)
			
			(send connection (login-client-packet/login-auth (list
				(cons 'login login)
				(cons 'password password)
				(cons 'session-id (get-box-field connection 'session-id))
				(cons 'rsa-key (get-box-field connection 'rsa-key))
			)))
			
			(let loop ()
				(let ((buffer (receive connection)))
					(case (get-packet-id buffer)
						((#x01) (let ((packet (login-server-packet/login-fail buffer)))
							(display "login fail: ")
							(displayln (cdr (assoc 'reason packet))) ; TODO message
							(disconnect connection)
						))
						((#x03) (let ((packet (login-server-packet/login-ok buffer)))
							(set-box-field! connection 'login-key (get-field packet 'login-key))
							(send connection (login-client-packet/server-list (list
								(cons 'login-key (get-field packet 'login-key))
							)))
							(loop)
						))
						((#x04) (let ((packet (login-server-packet/server-list buffer)))
							(get-field packet 'list)
						))
						(else #f)
					)
				)
			)
		)
	)
)