(module logic racket/base
	(require
		"../packet/game/client/reply_join_alliance.scm"
		"../packet/game/client/reply_join_clan.scm"
		"../packet/game/client/reply_join_party.scm"
		"../packet/game/client/reply_be_friends.scm"
		"../packet/game/client/confirm.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide reply)

	(define (reply connection question accept?)
		(send-packet connection (case question
			((ask/join-alliance) (game-client-packet/reply-join-alliance accept?))
			((ask/join-clan) (game-client-packet/reply-join-clan accept?))
			((ask/join-party) (game-client-packet/reply-join-party accept?))
			((ask/be-friends) (game-client-packet/reply-be-friends accept?))
			((confirm/resurrect) (game-client-packet/confirm 1510 accept?))
		))
	)
)
