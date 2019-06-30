(module api racket/base
	(require
		racket/math
		(rename-in racket/contract (any all/c))
		"../library/structure.scm"
		"../library/geometry.scm"
		"../library/network.scm"
		"../model/creature.scm"
		"../packet/game/client/move_to_point.scm"
	)
	(provide (contract-out
		(move-behind (->* (connection? creature?) (integer?) boolean?))
	))

	(define (move-behind connection creature [range 20])
		(and (> range 0) ; Нельзя определить угол между одной и той же точкой
			(let ((from (@: connection 'world 'me 'position)) (to (@: creature 'position)))
				; TODO check if not already behind and in range: calculate angle/2d and distance/3d

				; Сначала преобразуем инвертированный угол в нормальный, затем добавляем три четверти окружности и нормализуем до одного оборота
				(let ((angle (+ (get-angle creature) (* 3/2 pi))))
					(send connection (game-client-packet/move-to-point from
						(point/2d->point/3d (circle-point (point/3d->point/2d to) range angle) (point/3d-z to))
					))
					#t
				)
			)
		)
	)
)
