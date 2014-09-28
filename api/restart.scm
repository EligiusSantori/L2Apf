(module api racket/base
	(require
		racket/contract
		"../library/network.scm"
		"../packet/game/client/restart.scm"
	)
	(provide restart)
	
	(define (restart connection)
		(send connection (game-client-packet/restart))
	)
)