(module logic racket/base
	(require
		"../packet/game/client/refresh_manor_list.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide refresh-manor)

	(define (refresh-manor connection)
		(send-packet connection (game-client-packet/refresh-manor-list))
	)
)
