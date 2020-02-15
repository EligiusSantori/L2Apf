(module system racket/base
	(provide game-server-packet/character-list)
	(require "../../packet.scm")
	
	(define (read-item s n)
		(let ((name (read-utf16 s)))
			(begin
				(read-bytes 4 s) ; object-id
				(read-utf16 s) ; login
				(read-bytes (+ 4 4 4 4 4 4 4 12 8 8 4 4 4 4 36 64 64 4 4 4 8 8 4 4 4 1) s) ; other
				(list
					(cons 'id n)
					(cons 'name name)
				)
			)
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
	
	(define (game-server-packet/character-list buffer)
		(let ((s (open-input-bytes buffer)))
			(let ((id (read-byte s)) (count (read-int32 #f s)))
				(list
					(cons 'id id)
					(cons 'list (reverse (read-list s count 0 (list))))
				)
			)
		)
	)
)