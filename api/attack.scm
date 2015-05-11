(module api racket/base
	(require "interact.scm")
	(provide attack)
	
	(define (parse-args args)
		...
	)
	
	(define (attack connection . tail)
		(let-values (((object-id shift) (parse-args tail)))
			(interact object-id #t shift)
		)
	)
)