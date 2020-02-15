(module system racket/base
	(provide private-store)

	(define private-store (list
		(cons 0 'private-store/none)
		(cons 1 'private-store/sell-ready)
		(cons 2 'private-store/sell-manage)
		(cons 3 'private-store/buy-ready)
		(cons 4 'private-store/buy-manage)
		(cons 5 'private-store/manufacture)
	))
)