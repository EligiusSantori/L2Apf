(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/attack-stop)

	(define (game-server-packet/attack-stop buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
			)
		)
	)
)
