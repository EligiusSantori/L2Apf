(module logic racket/base
	(require
		"../packet/game/client/refresh_inventory.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide refresh-inventory)

	(define (refresh-inventory connection)
		(send-packet connection (game-client-packet/refresh-inventory))
	)
)
