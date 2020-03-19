(module logic racket/base
	(require
		"../packet/game/client/change_move_type.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide walk)

	(define (walk cn is?)
		(send-packet cn (game-client-packet/change-move-type (not is?)))
	)
)
