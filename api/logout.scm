(module logic racket/base
	(require
		"../packet/game/client/logout.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide logout)

	(define (logout connection) ; Не может быть синхронным, т.к. можно пропустить некоторые пакеты и получить отказ
		(send-packet connection (game-client-packet/logout))
	)
)
