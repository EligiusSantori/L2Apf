(module logic racket/base
	(require
		racket/math
		(rename-in racket/contract (any all/c))
		"../library/geometry.scm"
		"../packet/game/client/move_to_point.scm"
		"../model/creature.scm"
		"../model/world.scm"
		"../system/structure.scm"
		"../system/connection.scm"
	)
	(provide (contract-out
		(move-behind (->* (connection? creature?) (integer?) boolean?))
	))

	(define (move-behind cn creature [range 20])
		(and (> range 0) ; Нельзя определить угол между одной и той же точкой
			(let ((from (ref (world-me (connection-world cn)) 'position)) (to (ref creature 'position)))
				; TODO check if not already behind and in range: calculate angle/2d and distance/3d

				; Сначала преобразуем инвертированный угол в нормальный, затем добавляем три четверти окружности и нормализуем до одного оборота
				(let ((angle (+ (get-angle creature) (* 3/2 pi))))
					(send-packet cn (game-client-packet/move-to-point from
						(point/2d->point/3d (circle-point (point/3d->point/2d to) range angle) (point/3d-z to))
					))
					#t
				)
			)
		)
	)
)
