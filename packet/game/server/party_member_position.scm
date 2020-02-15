(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/party-member-position)
	
	(define (read-member s)
		(list
			(cons 'object-id (read-int32 #f s))
			(cons 'position (read-point s))

		)
	)
	
	(define (read-members s c n l)
		(if (< n c)
			(let ((i (read-member s)))
				(read-members s c (+ n 1) (cons i l))
			)
			l
		)
	)
	
	(define (game-server-packet/party-member-position buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'members (read-members s (read-int32 #f s) 0 (list)))
			)
		)
	)
)
