(module system racket/base
	(require
		"../../packet.scm"
		"../cubics.scm"
		"../mount_type.scm"
		"../private_store.scm"
	)
	(provide game-server-packet/char-info)

	(define (game-server-packet/char-info buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'position (read-point s))
				(cons 'angle (heading->angle (read-int32 #f s)))
				(cons 'object-id (read-int32 #f s))
				(cons 'name (read-utf16 s))
				(cons 'race (read-int32 #f s))
				(cons 'gender (if (zero? (read-int32 #f s)) 'gender/male 'gender/female))
				(cons 'class-id (read-int32 #f s))
				(cons 'clothing (list
					(cons 'underwear (read-int32 #f s))
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
				))
				(cons 'pvp? (not (zero? (read-int32 #f s))))
				(cons 'karma (read-int32 #f s))
				(cons 'magical-attack-speed (read-int32 #f s))
				(cons 'physical-attack-speed (read-int32 #f s))
				(cons 'run-speed (begin
					(read-int32 #f s) ; pvp? again
					(read-int32 #f s) ; karma again
					(read-int32 #f s)
				))
				(cons 'walk-speed (read-int32 #f s))
				(cons 'swim-run-speed (read-int32 #f s))
				(cons 'swim-walk-speed (read-int32 #f s))
				(cons 'fly-run-speed (begin
					(read-int32 #f s) ; FlRunSpd
					(read-int32 #f s) ; FlWalkSpd
					(read-int32 #f s)
				))
				(cons 'fly-walk-speed (read-int32 #f s))
				(cons 'move-speed-factor (read-double s))
				(cons 'attack-speed-factor (read-double s))
				(cons 'collision-radius (read-double s))
				(cons 'collision-height (read-double s))
				(cons 'hair-style (read-int32 #f s))
				(cons 'hair-color (read-int32 #f s))
				(cons 'face-type (read-int32 #f s))
				(cons 'title (read-utf16 s))
				(cons 'clan-id (read-int32 #f s))
				(cons 'clan-crest-id (read-int32 #f s))
				(cons 'ally-id (read-int32 #f s))
				(cons 'ally-crest-id (read-int32 #f s))

				(cons 'sitting? (begin
					(read-int32 #f s)
					(zero? (read-byte s))
				))
				(cons 'walking? (zero? (read-byte s)))
				(cons 'in-combat? (not (zero? (read-byte s))))
				(cons 'alike-dead? (not (zero? (read-byte s))))
				(cons 'invisible? (not (zero? (read-byte s))))
				(cons 'mount-type (cdr (assoc (read-byte s) mount-type)))
				(cons 'private-store (cdr (assoc (read-byte s) private-store)))
				(cons 'cubics (read-cubics s))
				(cons 'find-party? (not (zero? (read-byte s))))
				(cons 'recommendations-left (begin
					(read-int32 #f s) ; AbnormalEffects
					(read-byte s)
				))
				(cons 'recommendations-amount (read-int16 #f s))
				(cons 'max-cp (begin
					(read-int32 #f s) ; SpecialEffects
					(read-int32 #f s)
				))
				(cons 'cp (read-int32 #f s))
				(cons 'enchant (read-byte s))
				(cons 'large-crest-id (begin
					(read-byte s) ; TeamCircle
					(read-int32 #f s)
				))
				(cons 'hero-icon? (not (zero? (read-byte s))))
				(cons 'hero-glow? (not (zero? (read-byte s))))
				(cons 'fishing? (not (zero? (read-byte s))))
				(cons 'fish (read-point s))
				(cons 'name-color (read-int32 #f s))
			)
		)
	)
)
