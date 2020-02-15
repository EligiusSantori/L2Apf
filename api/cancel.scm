(module logic racket/base
	(require
		racket/contract
		"../system/network.scm"
		"../packet/game/client/cancel.scm"
	)
	(provide cancel)

	(define (cancel connection)
		(send connection (game-client-packet/cancel))
	)
)
