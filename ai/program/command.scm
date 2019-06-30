(module ai racket/base
	(require
		srfi/1
		racket/string
		racket/undefined
		"program.scm"
		"follow.scm"
		(relative-in "../../.."
			"library/extension.scm"
			"library/structure.scm"
			"model/world.scm"
			"api/say.scm"
			"api/gesture.scm"
			"api/logout.scm"
			"ai/manager.scm"
		)
	)
	(provide
		ai-program-command ; Execute commands from game chat
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

	(define ai-program-command (struct-copy ai-program ai-program-base
		[id 'command]
		[iterator (lambda (event connection config state) (let-values (((manager prefix) (list->values config)))
			(when (and event (eq? (car event) 'message))
				(let-values (((author-id channel author text) (apply values (cdr event))))
					(let ((command (parse-command text prefix))) (when command
						(case (car command)
							(("follow") (ai-manager-do! manager (ai-program-make ai-program-follow author-id 'repeat)))
							(("relax") (ai-manager-clear! manager #t))
							(("hello") (gesture connection 'gesture/hello))
							(("bye")
								(ai-manager-clear! manager #t) ; Call foreground program destructor before exit.
								(logout connection)
							)
							(else (say connection "I don't understand"))
						)
					))
				)
			)
			(void)
		))]
		[config (list
			undefined ; program manager (required)
			"/" ; command prefix
		)]
	))
)
