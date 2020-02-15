(module logic racket/base
	(require
		"../system/network.scm"
		"../packet/game/client/refresh_manor_list.scm"
	)
	(provide refresh-manor-list)

	(define (refresh-manor-list connection)
		(send connection (game-client-packet/refresh-manor-list))
	)
)
