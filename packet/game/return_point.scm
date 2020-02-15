(module system racket/base
	(provide return-points)

	(define return-points (list
		(cons 0 'return-point/town)
		(cons 1 'return-point/clanhall)
		(cons 2 'return-point/castle)
		(cons 3 'return-point/siege-hq)
		(cons 4 'return-point/fixed)
	))
)
