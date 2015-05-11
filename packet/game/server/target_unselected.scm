(module packet racket/base
	(require "../../packet.scm")
	(provide game-server-packet/target-unselected)
	
	(define (game-server-packet/target-unselected buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
				(cons 'position (read-point s))
			)
		)
	)
)