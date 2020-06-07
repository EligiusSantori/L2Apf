(module logic racket/base
	(require
		racket/math
		(rename-in racket/contract (any all/c))
		"../packet/game/client/move_to_point.scm"
		"../model/map.scm"
		"../model/creature.scm"
		"../model/world.scm"
		"../system/connection.scm"
	)
	(provide (contract-out
		(move-behind (->* (connection? creature?) (integer? (or/c rational? false/c)) boolean?))
	))

	(define (move-behind cn creature [range 20] [angle #f])
		(and (> range 0) ; Нельзя определить угол между одной и той же точкой
			(let* ((wr (connection-world cn)) (from (get-position (world-me wr))) )
				; TODO check if not already behind and in range: calculate angle/2d and distance/3d
				(let ((to (map-circle-point (get-position creature) range (or angle (+ pi (get-angle wr creature))))))
					(send-packet cn (game-client-packet/move-to-point from to))
					#t
				)
			)
		)
	)
)
