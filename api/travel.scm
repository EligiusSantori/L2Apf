; travel должен выполнять цепочку точек и всё! А прямой маршрут расчитывать надстройкой или на лету.

(module api racket/base
	(require
		"../library/structure.scm"
		"../library/geometry.scm"
		"../packet/game/client/move_to_point.scm"
	)
	(provide travel)
	
	(define interval 1000)
	
	(define (travel connection to)
		(define (move-to from to)
			(send connection (game-client-packet/move-to-point from to))
		)

		(let ((from (@: connection 'world 'me 'position)))
			(let ((count (/ (distance/3d from to) interval)))
				(if (> count 1)
					(let ((chain (box (split-line-segment/3d from to count))))
						(let ((proxy (set-proxy-event! connection 'change-moving (lambda (event)
							(when (equal? (@: event 'action) 'stop)
								(if (null? (cdr (unbox chain)))
									(set-event! connection proxy #f) ; remove event
									(begin
										(move-to (first (unbox chain)) (second (unbox chain)))
										(set-box! chain (cdr (unbox chain)))
									)
								)
							)
							
							(void)
						)))))
						
						(move-to (first (unbox chain)) (second (unbox chain)))
						(set-box! chain (cdr (unbox chain)))
					)

					(move-to from to)
				)
			)
		)
	)
)