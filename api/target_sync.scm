(module logic racket/base
	(require
		srfi/1
		"../system/structure.scm"
		"../system/contract.scm"
		"target.scm"
	)
	(provide target/sync)

	(define (target/sync connection object-id [shift? #f]) ; TODO accept object
		(define handle (make-contract target (lambda (event connection)
			(equal? (@: connection 'world 'me 'target-id) object-id)
		)))

		(when (not (equal? (@: connection 'world 'me 'target-id) object-id))
			(handle connection object-id shift?)
		)
	)
)
