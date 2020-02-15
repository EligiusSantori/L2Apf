(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/skill-started)
	
	(define (game-server-packet/skill-started buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
				(cons 'target-id (read-int32 #f s))
				(cons 'skill-id (read-int32 #f s))
				(cons 'level (read-int32 #f s))
				(cons 'cast-origin (read-int32 #f s))
				(cons 'reuse-delay (read-int32 #f s))
				(cons 'position
					(let ((position (read-point s)))
						(read-int16 #f s)
						position
					)
				)
			)
		)
	)
)
