; l2j/gameserver/serverpackets/Attack.java
(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/attack)

	(define (read-hit s)
		(let ((target-id (read-int32 #f s)) (damage (read-int32 #t s)) (flags (read-byte s)))
			(list
				(cons 'target-id target-id)
				(cons 'damage damage)
				(cons 'soulshot? (= (bitwise-and flags #x10) #x10))
				(cons 'critical? (= (bitwise-and flags #x20) #x20))
				(cons 'shield? (= (bitwise-and flags #x40) #x40))
				(cons 'miss? (= (bitwise-and flags #x80) #x80))
				(cons 'grade (list-ref
					(list 'ng 'd 'c 'b 'a 's)
					(bitwise-bit-field flags 0 4)
				))
			)
		)
	)

	(define (read-hits s c n l)
		(if (< n c)
			(let ((i (read-hit s)))
				(read-hits s c (+ n 1) (cons i l))
			)
			l
		)
	)

	(define (game-server-packet/attack buffer)
		(let ((s (open-input-bytes buffer)))
			(let ((id (read-byte s)) (object-id (read-int32 #f s)) (hit (read-hit s)))
				(list
					(cons 'id id)
					(cons 'object-id object-id)
					(cons 'position (read-point s))
					(cons 'hits (cons hit
						(read-hits s (read-int16 #f s) 0 (list))
					))
				)
			)
		)
	)
)
