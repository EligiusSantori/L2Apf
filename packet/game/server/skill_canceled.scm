(module packet racket/base
	(require "../../packet.scm")
	(provide game-server-packet/skill-canceled)
	
	(define (game-server-packet/skill-canceled buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
			)
		)
	)
)