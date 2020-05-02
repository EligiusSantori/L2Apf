(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/my-target-selected)

	(define (game-server-packet/my-target-selected buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'target-id (read-int32 #f s))
				(cons 'color (read-int16 #f s))
			)
		)
	)
)
