(module packet racket/base
	(provide loot-types)

	(define loot-types (list
		(cons 0 'party-loot/finder)
		(cons 1 'party-loot/random)
		(cons 2 'party-loot/random-all)
		(cons 3 'party-loot/by-turn)
		(cons 4 'party-loot/by-turn-all)
	))
)