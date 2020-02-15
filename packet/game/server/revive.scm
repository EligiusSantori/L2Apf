(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/revive)
	
	(define (game-server-packet/revive buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
			)
		)
	)
)
