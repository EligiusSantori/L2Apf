(module packet racket/base
	(provide actions)

	(define actions (list
		(cons 2 'social-action/hello)
		(cons 3 'social-action/victory)
		(cons 4 'social-action/charge)
		(cons 5 'social-action/no)
		(cons 6 'social-action/yes)
		(cons 7 'social-action/bow)
		(cons 8 'social-action/unaware)
		(cons 9 'social-action/waiting)
		(cons 10 'social-action/laugh)
		(cons 11 'social-action/applause)
		(cons 12 'social-action/dance)
		(cons 13 'social-action/sad)
		(cons 15 'social-action/level-up)
	))
)