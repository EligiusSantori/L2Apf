(module api racket/base
	(require
		racket/contract
		"../library/network.scm"
		"../packet/game/client/cancel.scm"
	)
	(provide cancel)
	
	(define (cancel connection)
		(send connection (game-client-packet/cancel))
	)
)