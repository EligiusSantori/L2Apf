(module packet racket/base
	(require
		racket/contract
		"../packet.scm"
	)
	(provide (contract-out
		(read-cubics (input-port? . -> . list?))
	))
	
	(define (read-cubics s)
		(define (r c l)
			(if (> c 0)
				(r (- c 1) (cons (read-int16 #f s) l))
				l
			)
		)
		(r (read-int16 #f s) (list))
	)
)