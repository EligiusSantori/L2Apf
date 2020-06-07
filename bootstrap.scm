(module script racket/base
	(require
		"system/structure.scm"
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
	(define (bootstrap host port account password name)
		(let ((connection (connect host port 656)))
			(let ((world (car (login connection account password))))
				(let ((me (findf (char-name=? name) (select-server connection world))))
					(let ((events (select-character connection me)))
						(values connection world me events)
					)
				)
			)
		)
	)
)
