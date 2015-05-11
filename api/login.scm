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
				(cons 'session-id (@: connection 'session-id))
				(cons 'rsa-key (@: connection 'rsa-key))
			)))
			
			(let loop ()
				(let ((buffer (receive connection)))
					(case (get-packet-id buffer)
						((#x01) (let ((packet (login-server-packet/login-fail buffer)))
							(disconnect connection)
							(error (string-append "Authentication failed: " (symbol->string (@: packet 'reason))))						
						))
						((#x03) (let ((packet (login-server-packet/login-ok buffer)))
							(set-box-field! connection 'login-key (@: packet 'login-key))
							(send connection (login-client-packet/server-list (list
								(cons 'login-key (@: packet 'login-key))
							)))
							(loop)
						))
						((#x04) (let ((packet (login-server-packet/server-list buffer)))
							(map make-hash (@: packet 'list))
						))
						(else #f)
					)
				)
			)
		)
	)
)