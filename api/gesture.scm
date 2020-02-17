(module logic racket/base
	(require
		"../packet/game/client/social_action.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide gesture)

	(define (gesture connection action)
		(send-packet connection (game-client-packet/social-action action))
	)
)
