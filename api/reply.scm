(module api racket/base
	(require
		"../library/network.scm"
		"../packet/game/client/reply_join_alliance.scm"
		"../packet/game/client/reply_join_clan.scm"
		"../packet/game/client/reply_join_party.scm"
		"../packet/game/client/reply_be_friends.scm"
	)
	(provide reply)
	
	(define (reply connection question accept?)
		(send connection (case question
			((ask/join-alliance) (game-client-packet/reply-join-alliance accept?))
			((ask/join-clan) (game-client-packet/reply-join-clan accept?))
			((ask/join-party) (game-client-packet/reply-join-party accept?))
			((ask/be-friends) (game-client-packet/reply-be-friends accept?))
		))
	)
)