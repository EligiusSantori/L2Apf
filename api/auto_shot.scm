(module logic racket/base
	(require
		racket/list
		"../packet/game/client/request_auto_shot.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide auto-shot)

	(define (get-item type grade)
		(let ((index (index-of (list 'ng 'd 'c 'b 'a 's) grade eq?)))
			(case type
				((soulshot) (list-ref (list 1835 1463 1464 1465 1466 1467) index))
				((spiritshot) (list-ref (list 2509 2510 2511 2512 2513 2514) index))
				((blessed-spiritshot) (list-ref (list 3947 3948 3949 3950 3951 3952) index))
				(else #f)
			)
		)
	)

	(define (auto-shot connection is? type grade)
		(let ((item-id (get-item type grade)))
			(when item-id
				(send-packet connection (game-client-packet/request-auto-shot item-id is?))
			)
		)
	)
)
