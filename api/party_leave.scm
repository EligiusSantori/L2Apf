(module logic racket/base
	(require
		"../packet/game/client/leave_party.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide party-invite)

	(define (party-leave cn)
		(send-packet cn (game-client-packet/leave-party))
	)
)
