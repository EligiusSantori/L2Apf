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
	(provide program-auto-confirm)

	(define-program program-auto-confirm
		(lambda (cn ev config . args)
			(case-event ev
				('confirm/resurrect (name exp)
					(reply cn (event-name ev) ((list-ref config 0) name exp))
				)
			)

			(void)
		)

		#:defaults (list
			(const #t) ; resurrect
		)
	)
)
