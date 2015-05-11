(module contract racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c) (make-contract make-contract/c))
		(only-in "timers.scm" set-timeout!)
		"../library/structure.scm"
	)
	(provide (contract-out
		(make-contract (->* (procedure? procedure?) #:rest (or/c false/c (listof any/c)) procedure?))
	))

	; TODO make-contract => make-sync-api
	(define (make-contract fn checker . tail)
		(define handler (if (> (length tail) 0) (first tail) values))
		(define timeout (if (> (length tail) 1) (/ (second tail) 1000) #f))
		
		(lambda (connection . args)
			(let ((events (@: connection 'event-channel)) (timeout-event (gensym)))
				(when timeout (set-timeout! connection timeout-event timeout))
				(apply fn (cons connection args))
			
				(let loop ()
					(let ((event (sync events)))
						(cond
							((equal? (@: event 'name) timeout-event) (handler #f))
							((checker event) (handler event))
							(else (loop))
						)
					)
				)
			)
		)
	)
)