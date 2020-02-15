(module logic racket/base
	(require
		racket/contract
		"../system/network.scm"
		"../packet/game/client/restart.scm"
	)
	(provide restart)

	(define (restart connection)
		(send connection (game-client-packet/restart))
	)
)
