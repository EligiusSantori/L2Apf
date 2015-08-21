(module packet racket/base
	(require "../../packet.scm")
	(provide game-server-packet/change-wait-type)
	
	(define (game-server-packet/change-wait-type buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
				(cons 'sitting? (zero? (read-int32 #f s)))
				(cons 'position (read-point s))
			)
		)
	)
)
