(module ai racket/base
	(require
		srfi/1
		racket/string
		racket/undefined
		"program.scm"
		"follow.scm"
		"brain.scm"
		(relative-in "../.."
			"library/extension.scm"
			"system/structure.scm"
			"model/world.scm"
			"api/say.scm"
			"api/gesture.scm"
			"api/logout.scm"
		)
	)
	(provide
		program-command ; Execute commands from game chat
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
					(ref (or (find-character-by-name (@: connection 'world) token) (list)) 'object-id)
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

	; TODO parse(command) e.g. (parse "follow [me]|my|<name>") ; string -> verb, [noun], [noun]
	; TODO get-targets(command) ; string -> list of object-id

	(define (command event connection config state) (let-values (((brain prefix) (list->values config)))
		(when (and event (eq? (car event) 'message))
			(let-values (((author-id channel author text) (apply values (cdr event))))
				(let ((command (parse-command text prefix))) (when command
					(case (car command)
						(("follow") (brain-do! brain (program-make program-follow author-id 'repeat)))
						(("relax") (brain-clear! brain #t))
						(("hello") (gesture connection 'gesture/hello))
						(("bye")
							(brain-clear! brain #t) ; Call foreground program destructor before exit.
							(logout connection)
						)
						(else (say connection "I don't understand"))
					)
				))
			)
		)
		(void)
	))

	(define-program program-command
		(list
			undefined ; program manager (required)
			"/" ; command prefix
		)
		undefined
		undefined
		command
	)
)
