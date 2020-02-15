(module system racket/base
	(provide game-server-packet/player-character)
	(require
		"../../../library/geometry.scm"
		"../../packet.scm"
		"../clothing.scm"
	)
	
	(define (game-server-packet/player-character buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'me (list
					(cons 'name (read-utf16 s))
					(cons 'title (begin
						(read-int32 #f s) ; must be object-id, but trash
						(read-utf16 s)
					))
					(cons 'session-id (read-int32 #f s))
					(cons 'clan-id (read-int32 #f s))
					(cons 'gender (begin
						(read-int32 #f s)
						(if (= (read-int32 #f s) 0) 'gender/male 'gender/female)
					))
					(cons 'race (read-int32 #f s))
					(cons 'base-class-id (read-int32 #f s))
					(cons 'is-active (not (= (read-int32 #f s) 0)))
					(cons 'position (point/3d
						(read-int32 #t s)
						(read-int32 #t s)
						(read-int32 #t s)
					))
					(cons 'hp (read-double s))
					(cons 'mp (read-double s))
					(cons 'sp (read-int32 #f s))
					(cons 'xp (read-int32 #f s))
					(cons 'level (read-int32 #f s))
					(cons 'karma (read-int32 #f s))
					
					(cons 'statements (list
						(cons 'INT (begin
							(read-int32 #f s)
							(read-int32 #f s)
						))
						(cons 'STR (read-int32 #f s))
						(cons 'CON (read-int32 #f s))
						(cons 'MEN (read-int32 #f s))
						(cons 'DEX (read-int32 #f s))
						(cons 'WIT (read-int32 #f s))
					))
					
					(cons 'equipment (begin
						(read-int32 #f s)
						(read-int32 #f s)
						(read-clothing s)
					))
					
					(cons 'clothing (read-clothing s))
					
					; TODO other data
				))
			)
		)
	)
)