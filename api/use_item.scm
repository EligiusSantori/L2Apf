(module logic racket/base
	(require
		"../packet/game/client/request_item_use.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide use-item)

	(define (use-item connection object-id)
		(send-packet connection (game-client-packet/request-item-use object-id))
	)
)
