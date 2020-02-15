(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/party-delete-member)
	
	(define (game-server-packet/party-delete-member buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
				(cons 'name (read-utf16 s))
			)
		)
	)
)
