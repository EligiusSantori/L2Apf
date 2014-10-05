(module system racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
	)
	(provide (contract-out
		(make-event (->* (symbol?) #:rest (or/c false/c list?) list?))
	))
	
	(define (make-event name . tail)
		(define data (if (null? tail) (list) (car tail)))
		(let ((data (if data data (list))))
			(alist-cons 'name name data)
		)
	)
)
