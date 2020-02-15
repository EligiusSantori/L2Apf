(module system racket/base
	(provide login-server-packet/login-ok)
	(require "../../packet.scm")
	
	(define (login-server-packet/login-ok buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'login-key (read-bytes 8 s))
			)
		)
	)
)