(module ai racket/base
	(require
		(only-in racket/function const)
		racket/undefined
		"program.scm"
		(relative-in "../."
			"system/structure.scm"
			"system/connection.scm"
			"system/event.scm"
			"api/reply.scm"
		)
	)
	(provide make-program-auto-confirm)

	(define (make-program-auto-confirm [resurrect? (const #t)])
		(make-program 'program-auto-confirm
			(lambda (cn ev . args)
				(case-event ev
					('confirm/resurrect (name exp)
						(reply cn (event-name ev) (resurrect? name exp))
					)
				)

				(void)
			)
		)
	)
)
