(module system racket/base
	(provide mount-type)
	
	(define mount-type (list
		(cons 0 'mount-type/none)
		(cons 1 'mount-type/strider)
		(cons 2 'mount-type/wyvern)
	))
)