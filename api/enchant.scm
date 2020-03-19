(module logic racket/base
	(require
		"../packet/game/client/use_item.scm"
		"../packet/game/client/enchant_item.scm"
		(only-in "../system/connection.scm" send-packet)
		(only-in "../system/event.scm" next-tick!)
	)
	(provide enchant)

	(define (enchant cn what-id with-id)
		(send-packet cn (game-client-packet/use-item with-id #f))
		(next-tick! cn (lambda args
			(send-packet cn (game-client-packet/enchant-item what-id))
		))
	)
)
