(module packet racket/base
	(require "../../packet.scm")
	(provide game-server-packet/party-all-members)
	
	(define (read-member s)
		(list
			(cons 'object-id (read-int32 #f s))
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
	
	(define (read-members s c n l)
		(if (< n c)
			(let ((i (read-member s)))
				(read-members s c (+ n 1) (cons i l))
			)
			l
		)
	)
	
	(define (read-data s)
		(list
			(cons 'id (read-byte s))
			(cons 'leader-id (read-int32 #f s))
			(cons 'loot-mode (read-int32 #f s))
			(cons 'members (read-members s (read-int32 #f s) 0 (list)))
		)
	)
	
	(define (game-server-packet/party-all-members buffer)
		(let ((s (open-input-bytes buffer)))
			(let ((_data (read-data s)))
				(begin
					(read-int32 #t s) ; skip
					(read-int32 #t s) ; skip
					_data
				)
			)
		)
	)
)
