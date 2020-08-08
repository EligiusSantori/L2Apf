(module ai racket/base
	(require
		racket/undefined
		"program.scm"
	)
	(provide make-program-idle)

	(define (make-program-idle)
		(make-program 'program-idle
			(lambda args (void)) ; Do nothing
		)
	)
)
