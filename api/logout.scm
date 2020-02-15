(module logic racket/base
	(require
		racket/contract
		"../system/network.scm"
		"../packet/game/client/logout.scm"
	)
	(provide logout)

	(define (logout connection) ; Не может быть синхронным, т.к. можно пропустить некоторые пакеты и получить отказ
		(send connection (game-client-packet/logout))
	)
)
