(module api racket/base
	(require
		racket/contract
		"../library/network.scm"
		"../packet/game/client/change_move_type.scm"
	)
	(provide run)
	
	(define (run connection is?)
		(send connection (game-client-packet/change-move-type is?))
	)
)