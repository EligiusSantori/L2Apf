(module logic racket/base
	(require
		"../packet/game/client/ask_join_party.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide party-invite)

	(define (party-invite cn name [loot 'finder])
		(send-packet cn (game-client-packet/ask-join-party name loot))
	)
)
