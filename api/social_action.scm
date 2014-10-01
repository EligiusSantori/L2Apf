(module api racket/base
	(require
		"../library/network.scm"
		"../packet/game/client/social_action.scm"
	)
	(provide social-action)
	
	(define (social-action connection action)
		(send connection (game-client-packet/social-action action))
	)
)