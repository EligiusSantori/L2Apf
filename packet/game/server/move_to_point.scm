(module packet racket/base
	(require "../../packet.scm")
	(provide game-server-packet/move-to-point)
	
	(define (game-server-packet/move-to-point buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
				(cons 'destination (read-point s))
				(cons 'position (read-point s))
			)
		)
	)
)
