; l2j/gameserver/serverpackets/UserInfo.java
(module system racket/base
	(require
		"../../packet.scm"
		"../race.scm"
		"../cubics.scm"
		"../clothing.scm"
		"../mount_type.scm"
		"../private_store.scm"
	)
	(provide game-server-packet/user-info)

	(define (read-base s)
		(list
			(cons 'id (read-byte s))
			(cons 'position (read-point s))
			(cons 'angle (heading->angle (read-int32 #f s)))
			(cons 'object-id (read-int32 #f s))
			(cons 'name (read-utf16 s))
			(cons 'race (cdr (assoc (read-int32 #f s) races)))
			(cons 'gender (if (zero? (read-int32 #f s)) 'gender/male 'gender/female))
			(cons 'base-class-id (read-int32 #f s))
			(cons 'level (read-int32 #f s))
			(cons 'xp (read-int32 #f s))
			(cons 'statements (list
				(cons 'STR (read-int32 #f s))
				(cons 'DEX (read-int32 #f s))
				(cons 'CON (read-int32 #f s))
				(cons 'INT (read-int32 #f s))
				(cons 'WIT (read-int32 #f s))
				(cons 'MEN (read-int32 #f s))
			))
			(cons 'max-hp (read-int32 #f s))
			(cons 'hp (read-int32 #f s))
			(cons 'max-mp (read-int32 #f s))
			(cons 'mp (read-int32 #f s))
			(cons 'sp (read-int32 #f s))
			(cons 'load (read-int32 #f s))
			(cons 'max-load (read-int32 #f s))
			(cons 'equipment (begin
					(read-int32 #f s)
					(read-clothing s)
			))
			(cons 'clothing (read-clothing s))

			(cons 'physical-attack-power (read-int32 #f s))
			(cons 'physical-attack-speed (read-int32 #f s))
			(cons 'physical-defense (read-int32 #f s))
			(cons 'evasion (read-int32 #f s))
			(cons 'accuracy (read-int32 #f s))
			(cons 'focus (read-int32 #f s))
			(cons 'magical-attack-power (read-int32 #f s))
			(cons 'magical-attack-speed (read-int32 #f s))
			(cons 'magical-defense (begin
				(read-int32 #f s)
				(read-int32 #f s)
			))
			(cons 'pvp? (not (zero? (read-int32 #f s))))
			(cons 'karma (read-int32 #f s))
			(cons 'run-speed (read-int32 #f s))
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
			(cons 'access-level (read-int32 #t s))
			(cons 'title (read-utf16 s))
			(cons 'clan-id (read-int32 #f s))
			(cons 'clan-crest-id (read-int32 #f s))
			(cons 'ally-id (read-int32 #f s))
			(cons 'ally-crest-id (read-int32 #f s))
			(cons 'clan-leader? (not (zero? (read-int32 #f s))))
			(cons 'mount-type (cdr (assoc (read-byte s) mount-type)))
			(cons 'private-store (cdr (assoc (read-byte s) private-store)))
			(cons 'dwarven-craft? (not (zero? (read-byte s))))
			(cons 'pk-count (read-int32 #f s))
			(cons 'pvp-count (read-int32 #f s))
			(cons 'cubics (read-cubics s))
			(cons 'find-party? (not (zero? (read-byte s))))
		)
	)

	(define (game-server-packet/user-info buffer)
		(let ((s (open-input-bytes buffer)))
			(append
				(read-base s)
				(parse-abnormal-effects (read-int32 #f s))
				(list
					(cons 'recommendations-left (begin
						(read-byte s)
						(read-int32 #f s) ; ClanPrivileges
						(read-bytes (* 4 7) s)
						(read-int16 #f s)
					))
					(cons 'recommendations-amount (read-int16 #f s))
					(cons 'inventory-limit (begin
						(read-int32 #f s) ; MountNpcId
						(read-int16 #f s)
					))
					(cons 'class-id (read-int32 #f s))
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
					(cons 'hero-icon? (not (zero? (read-byte s)))) ; TODO hero?
					(cons 'hero-glow? (not (zero? (read-byte s)))) ; TODO noble?
					(cons 'fishing? (not (zero? (read-byte s))))
					(cons 'fish (read-point s))
					(cons 'name-color (read-int32 #f s))
				)
			)
		)
	)
)
