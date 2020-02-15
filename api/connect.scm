(module logic racket/base
	(require
		(except-in racket/contract any)
		srfi/1
		racket/tcp
		"../library/blowfish.scm"
		"../system/structure.scm"
		"../system/network.scm"
		"../packet/login/server/init.scm"
		"../packet/login/client/gg_auth.scm"
		"../packet/login/server/gg_reply.scm"
	)
	(provide connect)

	(define (connect host [port 2106] [token #"_;5.]94-31==-%xT!^[$"] [protocol 660])
		(define connection (box (list
			(cons 'protocol protocol)
			(cons 'login-key (make-bytes 8 0)) ; Default value for short authentication.
		)))

		(let-values (((input-port output-port) (tcp-connect host port)))
			(set-box-field! connection 'input-port input-port)
			(set-box-field! connection 'output-port output-port)
			(let loop ()
				(let ((buffer (receive connection)))
					(case (get-packet-id buffer)
						((#x00) (let ((packet (login-server-packet/init buffer)))
							(set-box-field! connection 'session-id (@: packet 'session-id))
							(set-box-field! connection 'crypter (make-blowfish-crypter token))
							(set-box-field! connection 'rsa-key (@: packet 'rsa-key))

							(send connection (login-client-packet/gg-auth (list
								(cons 'session-id (@: packet 'session-id))
							)))

							(loop)
						))
						((#x0b) (let ((packet (login-server-packet/gg-reply buffer)))
							connection
						))
						(else #f)
					)
				)
			)
		)
	)
)
