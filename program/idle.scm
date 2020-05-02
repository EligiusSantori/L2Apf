(module ai racket/base
	(require
		racket/undefined
		"program.scm"
	)
	(provide
		program-idle
	)

	(define-program program-idle
		(lambda args (void)) ; Do nothing
	)
)
