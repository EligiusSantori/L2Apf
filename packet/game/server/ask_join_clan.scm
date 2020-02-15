(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/ask-join-clan)
	
	(define (game-server-packet/ask-join-clan buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
				(cons 'name (read-utf16 s))
			)
		)
	)
)
