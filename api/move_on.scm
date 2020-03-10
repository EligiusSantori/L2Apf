(module logic racket/base
	(require
		srfi/1
		"../library/geometry.scm"
		"../packet/game/client/move_to_point.scm"
		"../model/map.scm"
		"../model/creature.scm"
		"../model/world.scm"
		"../system/connection.scm"
	)
	(provide move-on)

	(define (move-on cn length [angle 0] [relative? #t]) ; TODO inverse angle if length is negative
		(let* ((wr (connection-world cn)) (me (world-me wr)) (from (get-position me)))
			(let ((angle (if relative? (+ (get-angle wr me) angle) angle)))
				(let ((to (map-circle-point from length angle)))
					(send-packet cn (game-client-packet/move-to-point from to))
				)
			)
		)
	)
)
