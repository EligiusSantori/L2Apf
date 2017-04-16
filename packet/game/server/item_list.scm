(module packet racket/base
	(require "../../packet.scm")
	(provide game-server-packet/item-list)
	
	(define (read-item s)
		(list
			(cons 'type1 (read-int16 #f s))
			(cons 'object-id (read-int32 #f s))
			(cons 'item-id (read-int32 #f s))
			(cons 'count (read-int32 #f s))
			(cons 'type2 (read-int16 #f s))
			(cons 'type3 (read-int16 #f s))
			(cons 'equipped? (not (zero? (read-int16 #f s))))
			(cons 'slot (read-int32 #f s))
			(cons 'enchant (read-int16 #f s))
			(cons 'type4 (read-int16 #f s))
		)
	)
	
	(define (read-items s c n l)
		(if (< n c)
			(let ((i (read-item s)))
				(read-items s c (+ n 1) (cons i l))
			)
			l
		)
	)
	
	(define (game-server-packet/item-list buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'open-window? (not (zero? (read-int16 #f s))))
				(cons 'items (read-items s (read-int16 #f s) 0 (list)))
			)
		)
	)
)
