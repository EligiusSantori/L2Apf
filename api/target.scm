(module api racket/base
	(require "interact.scm")
	(provide target)
	
	(define (target connection object-id . tail)
		(define shift (if (null? tail) #f (car tail)))
		(interact object-id #f shift)
	)
)