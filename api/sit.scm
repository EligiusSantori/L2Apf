(module api racket/base
	(require
		racket/contract
		"../library/network.scm"
		"../packet/game/client/change_wait_type.scm"
	)
	(provide sit)
	
	(define (sit connection is?)
		(send connection (game-client-packet/change-wait-type is?))
	)
)