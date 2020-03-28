(module logic racket/base
	(require
		"../packet/game/client/kick_party_member.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide party-kick)

	(define (party-kick cn name)
		(send-packet cn (game-client-packet/kick-party-memeber name))
	)
)
