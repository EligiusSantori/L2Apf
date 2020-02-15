(module logic racket/base
	(require
		srfi/1
		"../system/structure.scm"
		"../system/contract.scm"
		"cancel.scm"
	)
	(provide cancel/sync)

	(define (cancel/sync connection)
		(define handle (make-contract cancel (lambda (event connection)
			(equal? (@: connection 'world 'me 'target-id) #f)
		)))

		(when (@: connection 'world 'me 'target-id)
			(handle connection)
		)
	)
)
