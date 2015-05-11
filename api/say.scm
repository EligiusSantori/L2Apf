(module api racket/base
	(require
		"../library/network.scm"
		"../packet/game/client/say.scm"
	)
	(provide say)
	
	(define (parse-args args)
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
			(send connection (game-client-packet/say message channel target))
		)
	)
)