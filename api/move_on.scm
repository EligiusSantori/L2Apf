(module api racket/base
	(require
		srfi/1
		"../library/geometry.scm"
		"../library/structure.scm"
		"../library/network.scm"
		"../model/creature.scm"
		"../packet/game/client/move_to_point.scm"
	)
	(provide move-on)
	
	(define list-length length)
	
	(define (get-destination me angle length)
		(let ((from (@: me 'position)))
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
	
	(define (move-on connection length [_angle #f] [relative? #t]) ; TODO inverse angle if length is negative
		(let* ((me (@: connection 'world 'me)) (from (@: me 'position)))
			(let ((_angle (if relative? (+ (get-angle me) _angle) _angle)))
				(let ((to (get-destination me _angle length)))
					(send connection (game-client-packet/move-to-point from to))
				)
			)
		)
	)
)