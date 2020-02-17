(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
		"../library/geometry.scm"
		"../packet/game/client/move_to_point.scm"
		"../model/map.scm"
		"../model/creature.scm"
		"../model/world.scm"
		"../system/structure.scm"
		"../system/connection.scm"
	)
	(provide (contract-out
		(move-to (->* (connection? point/3d?) (integer?) (or/c point/3d? false/c)))
	))

	(define (move-to cn point [gap 0])
		(let* ((from (get-position (world-me (connection-world cn)))) (distance (points-distance from point)))
			(if (> distance gap)
				(let ((to (if (> gap 0) (segment-offset/3d point from gap) point)))
					(send-packet cn (game-client-packet/move-to-point from to))
					to
				)
				#f ; Point meaningless.
			)
		)
	)
)
