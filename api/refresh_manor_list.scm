(module logic racket/base
	(require
		"../packet/game/client/refresh_manor_list.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide refresh-manor-list)

	(define (refresh-manor-list connection)
		(send-packet connection (game-client-packet/refresh-manor-list))
	)
)
