(module api racket/base
	(require
		srfi/1
		"../library/structure.scm"
		"../system/contract.scm"
		"target.scm"
	)
	(provide target/sync)
	
	(define (target/sync connection object-id [shift? #f])
		(define handle (make-contract target (lambda (event connection)
			(equal? (@: connection 'world 'me 'target-id) object-id)
		)))
		
		(when (not (equal? (@: connection 'world 'me 'target-id) object-id))
			(handle connection object-id shift?)
		)
	)
)
