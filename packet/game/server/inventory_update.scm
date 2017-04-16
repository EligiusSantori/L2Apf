(module packet racket/base
	(require "../../packet.scm")
	(provide game-server-packet/inventory-update)
	
	(define (read-item s)
		(list
			(cons 'change (list-ref (list 'none 'add 'modify 'remove) (read-int16 #f s)))
			(cons 'type1 (read-int16 #f s))
			(cons 'object-id (read-int32 #f s))
			(cons 'item-id (read-int32 #f s))
			(cons 'count (read-int32 #f s))
			(cons 'type2 (read-int16 #f s))
			(cons 'type3 (read-int16 #f s))
			(cons 'equipped? (not (zero? (read-int16 #f s))))
			(cons 'slot (read-int32 #f s)) ; TODO symbols 0006-lr.ear, 0008-neck, 0030-lr.finger, 0040-head, 0100-l.hand, 0200-gloves, 0400-chest, 0800-pants, 1000-feet, 4000-r.hand, 8000-r.hand 
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
	
	(define (game-server-packet/inventory-update buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'items (read-items s (read-int16 #f s) 0 (list)))
			)
		)
	)
)
