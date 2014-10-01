(module system racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
	)
	(provide make-event)
	;(provide (contract-out
	;	(make-event (->* (symbol?) #:rest ((or/c list? )) list?))
	;))
	
	(define (make-event name . tail)
		(define data (if (null? tail) (list) (car tail)))
		(alist-cons 'name name data)
	)
)
