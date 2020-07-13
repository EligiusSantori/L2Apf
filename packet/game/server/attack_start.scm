(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/attack-start)

	(define (game-server-packet/attack-start buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
			)
		)
	)
)
