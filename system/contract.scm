(module system racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c) (make-contract make-contract/c))
		(only-in "timers.scm" set-timeout!)
		"structure.scm"
	)
	(provide (contract-out
		(make-contract (->* (procedure? procedure?) (procedure? (or/c integer? false/c)) procedure?))
	))

	; /TODO make-contract => make-sync-api
	;	TODO make-contract => syncronize((lambda() (api-fn connection)))
	(define (make-contract fn checker [handler values] [timeout #f])
		(lambda (connection . args)
			(let ((events (@: connection 'event-channel)) (timeout-event (gensym)))
				(when timeout (set-timeout! connection timeout-event (/ timeout 1000)))
				(apply fn (cons connection args))

				(let loop ()
					(let ((event (sync events)))
						(cond
							((equal? (car event) timeout-event) (handler #f))
							((checker event connection) (handler event connection))
							(else (loop))
						)
					)
				)
			)
		)
	)
)
