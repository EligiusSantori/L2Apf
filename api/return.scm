(module api racket/base
	(require
		racket/contract
		"../library/network.scm"
		"../packet/game/client/request_return.scm"
	)
	(provide return)
	
	(define (return connection [point 'return-point/town])
		(send connection (game-client-packet/request-return point))
	)
)
