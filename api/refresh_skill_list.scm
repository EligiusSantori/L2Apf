(module api racket/base
	(require
		"../library/network.scm"
		"../packet/game/client/refresh_skill_list.scm"
	)
	(provide refresh-skill-list)
	
	(define (refresh-skill-list connection)
		(send connection (game-client-packet/refresh-skill-list))
	)
)