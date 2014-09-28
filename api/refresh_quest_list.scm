(module api racket/base
	(require
		"../library/network.scm"
		"../packet/game/client/refresh_quest_list.scm"
	)
	(provide refresh-quest-list)
	
	(define (refresh-quest-list connection)
		(send connection (game-client-packet/refresh-quest-list))
	)
)