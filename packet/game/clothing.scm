(module system racket/base
	(require
		racket/contract
		racket/dict
		"../packet.scm"
	)
	(provide (contract-out
		(read-clothing (input-port? . -> . dict?))
	))

	(define (read-clothing s)
		(list
			(cons 'underwear (read-int32 #f s))
			(cons 'right-earing (read-int32 #f s))
			(cons 'left-earing (read-int32 #f s))
			(cons 'neck (read-int32 #f s))
			(cons 'right-finger (read-int32 #f s))
			(cons 'left-finger (read-int32 #f s))
			(cons 'head (read-int32 #f s))
			(cons 'right-hand (read-int32 #f s))
			(cons 'left-hand (read-int32 #f s))
			(cons 'gloves (read-int32 #f s))
			(cons 'chest (read-int32 #f s))
			(cons 'legs (read-int32 #f s))
			(cons 'feet (read-int32 #f s))
			(cons 'back (read-int32 #f s))
			(cons 'both-hand (read-int32 #f s))
			(cons 'hair (read-int32 #f s))
		)
	)
)