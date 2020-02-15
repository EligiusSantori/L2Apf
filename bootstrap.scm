(module script racket/base
	(require
		(rename-in racket/contract (any all/c))
		"system/structure.scm"
		"system/uri_scheme.scm"
		"model/creature.scm"
		"api/connect.scm"
		"api/login.scm"
		"api/select_server.scm"
		"api/select_character.scm"
	)
	(provide (contract-out
		(fighter? (creature? . -> . boolean?))
		(mystic? (creature? . -> . boolean?))
	))
	(provide
		bootstrap
		parse-protocol
	)

	(define (fighter? creature)
		(if (member (@: creature 'class-id) (list
			0 1 2 88 3 89 4 5 90 6 91 7 8 93 9 92 ; Human
			18 19 20 99 21 100 22 23 101 24 102 ; Dark Elf
			31 32 33 106 34 107 35 36 108 37 109 ; Elf
			44 45 46 113 47 48 114 ; Orc
			53 54 55 117 56 57 118 ; Dwarf
		)) #t #f)
	)

	(define (mystic? creature)
		(if (member (@: creature 'class-id) (list
			10 11 12 94 13 95 14 96 15 16 97 17 98 ; Human
			25 26 27 103 28 104 29 30 105 ; Dark Elf
			38 39 40 110 41 111 42 43 112 ; Elf
			49 50 51 115 52 116  ; Orc
		)) #t #f)
	)

	(define (parse-protocol [command-line (current-command-line-arguments)])
		(if (> (vector-length command-line) 0)
			(let ((uri (parse-uri (vector-ref command-line 0))))
				(if uri
					(apply values uri)
					(raise-user-error "Authentication failed because URI is broken.")
				)
			)
			(raise-user-error "Authentication failed because URI is missed.")
		)
	)

	(define (bootstrap host port account password name [entry #f])
		(let ((connection (connect host port)))
			(let ((world (findf (lambda (server) (@: server 'state)) (login connection account password))))
				(let ((me (cdr (assoc name (select-server connection world) string-ci=?))))
					(let ((events (select-character connection me)))
						(if entry
							(entry connection world me events)
							(values connection world me events)
						)
					)
				)
			)
		)
	)
)
