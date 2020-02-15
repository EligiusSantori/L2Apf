(module system racket/base
	(provide login-server-packet/play-fail)
	(require "../../packet.scm")
	
	(define (login-server-packet/play-fail buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'reason (case (read-byte s)
					((#x01) 'system-error)
					((#x0f) 'too-many-players)
				))
			)
		)
	)
)