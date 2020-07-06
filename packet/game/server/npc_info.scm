(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/npc-info)

	(define (game-server-packet/npc-info buffer)
		(let ((s (open-input-bytes buffer)))
			(append (list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
				(cons 'npc-id (- (read-int32 #f s) 1000000))
				(cons 'attackable? (not (zero? (read-int32 #f s))))
				(cons 'position (read-point s))
				(cons 'angle (heading->angle (read-int32 #f s)))
				(cons 'magical-attack-speed (begin
					(read-int32 #f s) ; ?
					(read-int32 #f s)
				))
				(cons 'physical-attack-speed (read-int32 #f s))
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
				(cons 'clothing (list
					(cons 'right-hand (read-int32 #f s))
					(cons 'both-hand (read-int32 #f s))
					(cons 'left-hand (read-int32 #f s))
				))
				(cons 'show-name? (not (zero? (read-byte s))))
				(cons 'walking? (zero? (read-byte s)))
				(cons 'in-combat? (not (zero? (read-byte s))))
				(cons 'alike-dead? (not (zero? (read-byte s))))
				(cons 'summoned? (not (zero? (read-byte s))))
				; (cons 'name (read-utf16 s)) ; Always empty, excluded.
				(cons 'title (begin (read-utf16 s) (read-utf16 s)))
			) (parse-abnormal-effects (begin
				(read-int32 #f s)
				(read-int32 #f s)
				(read-int32 #f s)
				(read-int32 #f s)
			)))
		)
	)
)
