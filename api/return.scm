(module logic racket/base
	(require
		racket/contract
		"../packet/game/client/request_return.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide return)

	(define (return connection [point 'return-point/town])
		(send-packet connection (game-client-packet/request-return point))
	)
)
