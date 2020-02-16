(module logic racket/base
	(require
		"../packet/game/client/refresh_quest_list.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide refresh-quest-list)

	(define (refresh-quest-list connection)
		(send-packet connection (game-client-packet/refresh-quest-list))
	)
)
