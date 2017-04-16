(module script racket/base
	(require
		srfi/1
		racket/string
		(rename-in racket/contract (any all/c))
		(relative-in "../.."
			"system/uri_scheme.scm"
			"library/extension.scm"
			"library/structure.scm"
			;"library/geometry.scm"
			"model/world.scm"
			"model/creature.scm"
			;"api/move_to.scm"
		)
	)
	(provide
		parse-protocol
		parse-command
		format-chat-message
		parse-targets
	)
	(provide (contract-out
		(fighter? (creature? . -> . boolean?))
		(mystic? (creature? . -> . boolean?))
	))
	
	(define (parse-protocol command-line)
		(if (> (vector-length command-line) 0)
			(let ((uri (parse-uri (vector-ref command-line 0))))
				(if uri
					(apply values uri)
					(error "Authentication failed because URI is broken")
				)
			)
			(error "Authentication failed because URI is missed")
		)
	)

	(define (parse-command text [opener "/"])
		(let ((t (string-trim (string-downcase text))))
			(if (string-starts? t opener)
				(let ((l (map string-trim (string-split t " "))))
					(cons (substring (car l) (string-length opener)) (cdr l))
				)
				#f
			)
		)
	)

	(define (format-chat-message object-id channel author text)
		(let ((channel (string-titlecase (last (string-split (symbol->string channel) "/")))))
			(string-append "[" channel "] " author ": " text)
		)
	)
	
	(define (parse-targets connection author . arguments)
		(define (parse-target token [default #f])
			(case (or token default)
				(("me") (@: (or author (list)) 'object-id))
				(("self") (@: connection 'world 'me 'object-id))
				(("my") (@: (or author (list)) 'target-id))
				(("us" "party")
					(displayln (@: connection 'world 'party))
					(list) ; TODO
				)
				(else (and (string? token)
					(@: (or (find-character-by-name (@: connection 'world) token) (list)) 'object-id)
				))
			)
		)
		
		(if (not (null? arguments))
			(fold (lambda (s p)
				(let ((t (parse-target s)))
					(append p (cond
						((integer? t) (list t))
						((not t) (list))
						(else t)
					))
				)
			) (list) arguments)
			(list (parse-target #f))
		)
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
)
