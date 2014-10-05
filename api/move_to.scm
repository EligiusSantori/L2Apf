(module api racket/base
	(require
		"../library/structure.scm"
		"../library/network.scm"
		"../packet/game/client/move_to_point.scm"
	)
	(provide move-to)
	
	(define (move-to connection point)
		(let ((from (@: connection 'world 'me 'position)))
			(send connection (game-client-packet/move-to-point from point))
		)
	)
)