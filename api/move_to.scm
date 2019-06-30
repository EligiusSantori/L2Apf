(module api racket/base
	(require
		(rename-in racket/contract (any all/c))
		"../library/structure.scm"
		"../library/geometry.scm"
		"../library/network.scm"
		"../packet/game/client/move_to_point.scm"
		"../model/map.scm"
		"../model/creature.scm"
	)
	(provide (contract-out
		(move-to (->* (connection? point/3d?) (integer?) (or/c point/3d? false/c)))
	))

	(define (move-to connection point [gap 0])
		(let* ((from (get-position (ref connection 'world 'me))) (distance (points-distance from point)))
			(if (> distance gap)
				(let ((to (if (> gap 0) (segment-offset/3d point from gap) point)))
					(send connection (game-client-packet/move-to-point from to))
					to
				)
				#f ; Point meaningless.
			)
		)
	)
)
