(module packet racket/base
	(provide game-server-packet/system-message)
	(require "../../packet.scm" "../argument.scm")
	
	(define (read-item s n)
		(let ((type (cdr (assoc (read-int32 #f s) arguments =))))
			(case type
				('text (cons type (read-utf16 s)))
				('number (cons type (read-int32 #t s)))
				('npc (cons type (read-int32 #f s)))
				('item (cons type (read-int32 #f s)))
				('skill (cons type (let ((skill-id (read-int32 #f s)))
					(read-int32 #f s)
					skill-id
				)))
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
	
	(define (game-server-packet/system-message buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'message-id (read-int32 #f s))
				(cons 'arguments (let ((count (read-int32 #f s)))
					(read-list s count 0 (list))
				))
			)
		)
	)
)