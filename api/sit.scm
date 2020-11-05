(module logic racket/base
	(require
		racket/contract
		"../packet/game/client/change_wait_type.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide sit)

	(define (sit connection is?)
		(begin (send-packet connection (game-client-packet/change-wait-type is?)) #t)
	)
)
