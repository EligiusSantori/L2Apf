(module packet racket/base
	(require "../../packet.scm")
	(provide game-server-packet/object-deleted)
	
	(define (game-server-packet/object-deleted buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
			)
		)
	)
)
