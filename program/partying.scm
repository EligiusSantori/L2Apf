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
		(list
			#f ; leader names filter
		)
		undefined
		undefined
		(lambda (cn event config . args)
			(when (and (eq? (car event) 'ask/join-party) (not (in-party? (world-party (connection-world cn)))))
				(let ((from (ref (cdr event) 'player)) (names (car config)))
					(reply cn (car event) (or (not names) (member from names string-ci=?)))
				)
			)
			(void)
		)
	)
)
