(module logic racket/base
	(require
		racket/contract
		"../packet/game/client/restart.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide restart)

	(define (restart connection)
		(send-packet connection (game-client-packet/restart))
	)
)
