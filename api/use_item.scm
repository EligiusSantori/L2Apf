(module api racket/base
	(require
		"../library/network.scm"
		"../packet/game/client/.scm"
	)
	(provide use-item)
	
	(define (use-item connection item-id)
		(send connection (game-client-packet/ item-id))
	)
)