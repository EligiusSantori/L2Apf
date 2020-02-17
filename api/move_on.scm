(module logic racket/base
	(require
		srfi/1
		"../library/geometry.scm"
		"../packet/game/client/move_to_point.scm"
		"../model/creature.scm"
		"../model/world.scm"
		"../system/structure.scm"
		"../system/connection.scm"
	)
	(provide move-on)

	(define list-length length)

	(define (get-destination me angle length)
		(let ((from (ref me 'position)))
			(let ((center (point/2d (point/3d-x from) (- (point/3d-y from)))))
				(let ((to (circle-point center length angle)))
					(point/3d
						(point/2d-x to)
						(- (point/2d-y to))
						(point/3d-z from)
					)
				)
			)
		)
	)

	(define (move-on cn length [_angle #f] [relative? #t]) ; TODO inverse angle if length is negative
		(let* ((me (world-me (connection-world cn))) (from (ref me 'position)))
			(let ((_angle (if relative? (+ (get-angle me) _angle) _angle)))
				(let ((to (get-destination me _angle length)))
					(send-packet cn (game-client-packet/move-to-point from to))
				)
			)
		)
	)
)
