(module packet racket/base
	(provide game-server-packet/skill-list)
	(require "../../packet.scm")
	
	(define (read-item s n)
		(list
			(cons 'active? (zero? (read-int32 #f s)))
			(cons 'level (read-int32 #f s))
			(cons 'skill-id (read-int32 #f s))
		)
	)
	
	(define (read-list s c n l)
		(if (< n c)
			(let ((i (read-item s n)))
				(read-list s c (+ n 1) (cons i l))
			)
			l
		)
	)
	
	(define (game-server-packet/skill-list buffer)
		(let ((s (open-input-bytes buffer)))
			(let ((id (read-byte s)) (count (read-int32 #f s)))
				(list
					(cons 'id id)
					(cons 'list (read-list s count 0 (list)))
				)
			)
		)
	)
)