(module system racket/base
	(provide login-server-packet/play-ok)
	(require "../../packet.scm")
	
	(define (login-server-packet/play-ok buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'game-key (read-bytes 8 s))
			)
		)
	)
)