(module ai racket/base
	(require
		"program.scm"
	)
	(provide
		ai-program-idle
	)

	(define ai-program-idle (struct-copy ai-program ai-program-base
		[id 'idle]
		[iterator (lambda args (void))] ; Do nothing
	))
)
