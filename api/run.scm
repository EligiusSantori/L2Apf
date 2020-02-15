(module logic racket/base
	(require
		racket/contract
		"../system/network.scm"
		"../packet/game/client/change_move_type.scm"
	)
	(provide run)

	(define (run connection is?)
		(send connection (game-client-packet/change-move-type is?))
	)
)
