(module packet racket/base
	(provide game-server-packet/player-character)
	(require "../../../library/geometry.scm")
	(require "../../packet.scm")
	
	(define (read-equipment s)
		(list
			(cons 'underwear (read-int32 #f s))
			(cons 'right-earing (read-int32 #f s))
			(cons 'left-earing (read-int32 #f s))
			(cons 'neck (read-int32 #f s))
			(cons 'right-finger (read-int32 #f s))
			(cons 'left-finger (read-int32 #f s))
			(cons 'head (read-int32 #f s))
			(cons 'right-hand (read-int32 #f s))
			(cons 'left-hand (read-int32 #f s))
			(cons 'gloves (read-int32 #f s))
			(cons 'chest (read-int32 #f s))
			(cons 'legs (read-int32 #f s))
			(cons 'feet (read-int32 #f s))
			(cons 'back (read-int32 #f s))
			(cons 'both-hand (read-int32 #f s))
			(cons 'hair (read-int32 #f s))
		)
	)
	
	(define (game-server-packet/player-character buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'me (list
					(cons 'name (read-utf16 s))
					(cons 'object-id (read-int32 #f s))
					(cons 'title (read-utf16 s))
					(cons 'session-id (read-int32 #f s))
					(cons 'clan-id (read-int32 #f s))
					(cons 'gender (begin
						(read-int32 #f s)
						(if (= (read-int32 #f s) 0) 'male 'female)
					))
					(cons 'race (read-int32 #f s))
					(cons 'base-class-id (read-int32 #f s))
					(cons 'is-active (not (= (read-int32 #f s) 0)))
					(cons 'position (point3d
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
					
					(cons 'INT (begin
						(read-int32 #f s)
						(read-int32 #f s)
					))
					(cons 'STR (read-int32 #f s))
					(cons 'CON (read-int32 #f s))
					(cons 'MEN (read-int32 #f s))
					(cons 'DEX (read-int32 #f s))
					(cons 'WIT (read-int32 #f s))
					
					(cons 'equipment (begin
						(read-int32 #f s)
						(read-int32 #f s)
						(read-equipment s)
					))
					
					(cons 'clothing (read-equipment s))
					
					; TODO other data
				))
			)
		)
	)
)