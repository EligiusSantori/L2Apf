(module logic racket/base
	(require
		"../packet/game/client/change_party_leader.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide party-crown)

	(define (party-crown cn name)
		(send-packet cn (game-client-packet/change-party-leader name))
	)
)
