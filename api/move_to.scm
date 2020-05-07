(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
		"../library/geometry.scm"
		"../packet/game/client/move_to_point.scm"
		"../packet/game/client/stop_moving.scm"
		"../model/map.scm"
		"../model/creature.scm"
		"../model/world.scm"
		"../system/structure.scm"
		"../system/connection.scm"
	)
	(provide (contract-out
		(move-to (->* (connection? (or/c point/3d? false/c)) (integer?) (or/c point/3d? false/c)))
	))

	(define (move-to cn point [gap 0])
		(let* ((wr (connection-world cn)) (me (world-me wr)) (from (get-position me)))
			(if point
				(if (> (points-distance from point) gap) ; Maybe start moving.
					(let ((to (if (> gap 0) (segment-offset/3d point from gap) point)))
						(send-packet cn (game-client-packet/move-to-point from to))
						to
					)
					#f ; Point meaningless.
				)
				(if (moving? me) ; Maybe stop moving.
					(begin
						(send-packet cn (game-client-packet/stop-moving from (get-angle wr me)))
						from
					)
					#f ; Action needless.
				)
			)
		)
	)
)
