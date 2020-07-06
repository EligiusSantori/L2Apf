(module script racket/base
	(require
		"system/structure.scm"
		"system/connection.scm"
		"api/connect.scm"
		"api/login.scm"
		"api/select_server.scm"
		"api/select_character.scm"
	)
	(provide
		bootstrap
	)

	(define (char-name=? name)
		(lambda (ch) (string-ci=? name (ref ch 'name)))
	)
	(define (bootstrap host port account password name [db #f])
		(let ((cn (connect host port 656)))
			(when db (set-connection-db! cn db))
			(let ((world (car (login cn account password))))
				(let ((me (findf (char-name=? name) (select-server cn world))))
					(let ((events (select-character cn me)))
						(values cn world me events)
					)
				)
			)
		)
	)
)
