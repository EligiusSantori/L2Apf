(module logic racket/base
	(require
		racket/contract
		"../packet/game/client/return.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide return)

	(define (return connection [point 'return-point/town])
		(send-packet connection (game-client-packet/return point))
	)
)
