(module packet racket/base
	(require "../../packet.scm")
	(provide game-server-packet/party-add-member)
	
	(define (game-server-packet/party-add-member buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (begin
					(read-int32 #t s) ; skip
					(read-int32 #t s) ; skip
					(read-int32 #f s)
				))
				(cons 'name (read-utf16 s))
				(cons 'cp (read-int32 #f s))
				(cons 'max-cp (read-int32 #f s))
				(cons 'hp (read-int32 #f s))
				(cons 'max-hp (read-int32 #f s))
				(cons 'mp (read-int32 #f s))
				(cons 'max-mp (read-int32 #f s))
				(cons 'level (read-int32 #f s))
				(cons 'class-id (read-int32 #f s))
			)
		)
	)
)
