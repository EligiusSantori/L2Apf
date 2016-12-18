(module packet racket/base
	(provide arguments)
	
	(define arguments (list
		(cons 0 'text)
		(cons 1 'number)
		(cons 2 'npc)
		(cons 3 'item)
		(cons 4 'skill)
	))
)