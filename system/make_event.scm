(module system racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
	)
	(provide (contract-out
		(make-event (->* (symbol?) #:rest list? list?))
	))
	
	(define (make-event name . data)
		(cons name data)
	)
)
