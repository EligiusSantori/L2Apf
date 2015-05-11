(module packet racket/base
	(require "../../packet.scm")
	(provide game-server-packet/target-selected)
	
	(define (game-server-packet/target-selected buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
				(cons 'target-id (read-int32 #f s))
				(cons 'position (read-point s))
			)
		)
	)
)