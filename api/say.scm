(module logic racket/base
	(require
		srfi/1
		"../packet/game/client/say.scm"
		"../packet/game/client/command_builder.scm"
		(only-in "../system/connection.scm" send-packet)
	)
	(provide say)

	(define (parse-args tail)
		(if (null? tail)
			(values 'chat-channel/all "")
			(cond
				((string? (car tail)) (values 'chat-channel/tell (car tail)))
				((> (length tail) 1) (values (first tail) (second tail)))
				((symbol? (car tail)) (values (car tail) ""))
			)
		)
	)

	(define (say connection message . tail)
		(let-values (((channel target) (parse-args tail)))
			(case channel
				((chat-channel/game-master)
					(send-packet connection (game-client-packet/command-builder message))
				)
				(else
					(send-packet connection (game-client-packet/say message channel target))
				)
			)
		)
	)
)
