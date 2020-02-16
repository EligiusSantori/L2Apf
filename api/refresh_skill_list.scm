(module logic racket/base
	(require
		"../packet/game/client/refresh_skill_list.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide refresh-skill-list)

	(define (refresh-skill-list connection)
		(send-packet connection (game-client-packet/refresh-skill-list))
	)
)
