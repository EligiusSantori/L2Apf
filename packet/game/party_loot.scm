(module system racket/base
	(provide loot-types)

	(define loot-types (list
		(cons 0 'finder)
		(cons 1 'random)
		(cons 2 'random-all)
		(cons 3 'by-turn)
		(cons 4 'by-turn-all)
	))
)
