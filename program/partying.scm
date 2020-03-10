(module ai racket/base
	(require
		racket/undefined
		"program.scm"
		(relative-in "../."
			"system/structure.scm"
			"system/connection.scm"
			"model/party.scm"
			"model/world.scm"
			"api/reply.scm"
		)
	)
	(provide program-partying)

	(define-program program-partying
		(list) ; leader names filter
		undefined
		undefined
		(lambda (cn event names . args)
			(when (and (eq? (car event) 'ask/join-party) (not (in-party? (world-party (connection-world cn)))))
				(let ((from (ref (cdr event) 'player)))
					(reply cn (car event)
						(or (null? names) (member from names string=?))
					)
				)
			)
			(void)
		)
	)
)
