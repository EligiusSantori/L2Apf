(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/teleport)
	
	(define (game-server-packet/teleport buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
				(cons 'position (read-point s))
			)
		)
	)
)
