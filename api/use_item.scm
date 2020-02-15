(module logic racket/base
	(require
		"../system/network.scm"
		"../packet/game/client/request_item_use.scm"
	)
	(provide use-item)

	(define (use-item connection object-id)
		(send connection (game-client-packet/request-item-use object-id))
	)
)
