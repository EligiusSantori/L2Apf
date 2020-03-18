(module script racket/base
	(require
		srfi/1
		racket/string
		(rename-in racket/contract (any all/c))
		(only-in yaml read-yaml)
		"library/extension.scm"
		"system/structure.scm"
		"system/uri_scheme.scm"
		"system/connection.scm"
		"system/event.scm"
		"model/creature.scm"
		"model/world.scm"
		"api/connect.scm"
		"api/login.scm"
		"api/select_server.scm"
		"api/select_character.scm"
		"api/logout.scm"
		"program/program.scm"
	)
	(provide (contract-out
		(fighter? (creature? . -> . boolean?))
		(mystic? (creature? . -> . boolean?))
	))
	(provide
		parse-protocol
		parse-config
		bootstrap
		terminate
	)

	(define (fighter? creature)
		(if (member (ref creature 'class-id) (list
			0 1 2 88 3 89 4 5 90 6 91 7 8 93 9 92 ; Human
			18 19 20 99 21 100 22 23 101 24 102 ; Dark Elf
			31 32 33 106 34 107 35 36 108 37 109 ; Elf
			44 45 46 113 47 48 114 ; Orc
			53 54 55 117 56 57 118 ; Dwarf
		)) #t #f)
	)

	(define (mystic? creature)
		(if (member (ref creature 'class-id) (list
			10 11 12 94 13 95 14 96 15 16 97 17 98 ; Human
			25 26 27 103 28 104 29 30 105 ; Dark Elf
			38 39 40 110 41 111 42 43 112 ; Elf
			49 50 51 115 52 116  ; Orc
		)) #t #f)
	)

	(define (parse-config [command-line (current-command-line-arguments)])
		(fold (lambda (arg cfg)
			(if (and (not cfg) (string-ends? (string-downcase arg) ".yaml") (file-exists? arg))
				(call-with-input-file arg read-yaml)
				cfg
			)
		) #f (vector->list command-line))
	)

	(define (parse-protocol [command-line (current-command-line-arguments)])
		(cond
			((and (> (vector-length command-line) 1) (file-exists? (vector-ref command-line 0))) ; Login via config file.
				(let ((config (call-with-input-file (vector-ref command-line 0) read-yaml)) (name (vector-ref command-line 1)))
					(values (or (ref config "host") "localhost") (or (ref config "port") 2106) name (ref config "password") name)
				)
			)
			((and (> (vector-length command-line) 0) (string-starts? (string-downcase (vector-ref command-line 0)) "l2apf:")) ; Login via URI.
				(let ((uri (parse-uri (vector-ref command-line 0))))
					(if uri
						(apply values uri)
						(raise-user-error "Authentication failed because URI is broken.")
					)
				)
			)
			(else (raise-user-error "Insufficient authentication data."))
		)
	)

	(define (char-name=? name)
		(lambda (ch) (string-ci=? name (ref ch 'name)))
	)

	(define (print-handler value port)
		(cond
			((bytes? value) ; Custom (hex) printer for byte string.
				(display "[" port)
				(display (string-join (map byte->hex (bytes->list value)) " ") port)
				(display "]" port)
			)
			((connection? value)
				(display "#<connection:" port)
				(display (connection-account value) port)
				(display ">" port)
			)
			((world? value)
				(display "#<world(" port)
				(display (hash-count (world-objects value)) port)
				(display "):" port)
				(display (ref (world-me value) 'name) port)
				(display ">" port)
			)
			((program? value)
				(display "#<program:" port)
				(display (program-id value) port)
				(display ">" port)
			)
			(else (write value port))
		)
	)

	(define (terminate connection events)
		(logout connection)
		(let loop ()
			(cond
				((eq? 'logout (event-name (sync events))) (displayln "Logged out."))
				(else (loop))
			)
		)
	)

	(define (bootstrap entry host port account password name)
		(global-port-print-handler print-handler)
		(let ((connection (connect host port)))
			(let ((world (first (login connection account password))))
				(let ((me (findf (char-name=? name) (select-server connection world))))
					(let ((events (select-character connection me)))
						(with-handlers ((exn:break? (lambda (e) (terminate connection events))))
							(entry connection world me events)
						)
					)
				)
			)
		)
	)
)
