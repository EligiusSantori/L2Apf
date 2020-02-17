(module logic racket/base
	(require
		racket/contract
		"../packet/game/client/change_move_type.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide run)

	(define (run connection is?)
		(send-packet connection (game-client-packet/change-move-type is?))
	)
)
