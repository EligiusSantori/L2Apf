(module system racket/base
	(provide login-server-packet/init)
	(require "../../packet.scm")
	
	(define (login-server-packet/init buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'session-id (read-int32 #f s))
				(cons 'protocol (read-int32 #f s))
				(cons 'rsa-key (scramble (read-bytes 128 s)))
			)
		)
	)
)