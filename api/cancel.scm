(module logic racket/base
	(require
		racket/contract
		"../packet/game/client/cancel.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide cancel)

	(define (cancel connection)
		(send-packet connection (game-client-packet/cancel))
	)
)
