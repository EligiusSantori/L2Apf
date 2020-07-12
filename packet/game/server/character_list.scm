; l2j/gameserver/serverpackets/CharSelectInfo.java
(module system racket/base
	(provide game-server-packet/character-list)
	(require
		racket/math
		"../../packet.scm"
		"../race.scm"
	)

	(define (read-item s n)
		(let ((name (read-utf16 s)))
			(begin
				(read-bytes 4 s) ; must be object-id, but trash
				(read-utf16 s) ; login
				(read-int32 #f s) ; session-id
				(read-int32 #f s) ; clan-id
				(read-int32 #f s) ; ?
				(list
					(cons 'character-id n)
					(cons 'name name)
					(cons 'gender (if (zero? (read-int32 #f s)) 'gender/male 'gender/female))
					(cons 'race (cdr (assoc (read-int32 #f s) races)))
					(cons 'base-class-id (read-int32 #f s))
					(cons 'hp (begin
						(read-bytes (+ 4 12) s)
						(exact-floor (read-double s))
					))
					(cons 'mp (exact-floor (read-double s)))
					(cons 'sp (read-int32 #f s))
					(cons 'xp (read-int32 #f s))
					(cons 'level (read-int32 #f s))
					(cons 'karma (read-int32 #f s))
					(cons 'class-id (begin
						(read-bytes (+ 36 64 64 4 4 4 8 8 4) s)
						(read-int32 #f s)
						; (read-bytes (+ 4 1) s)
					))
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
