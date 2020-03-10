(module ai racket/base
	(require
		srfi/1
		racket/string
		racket/undefined
		"program.scm"
		"follow_chase.scm"
		"follow_repeat.scm"
		"brain.scm"
		(relative-in "../."
			"library/extension.scm"
			"system/structure.scm"
			"system/connection.scm"
			"system/event.scm"
			"model/creature.scm"
			"model/party.scm"
			"model/world.scm"
			"api/say.scm"
			"api/gesture.scm"
			"api/move_to.scm"
			"api/move_on.scm"
			"api/move_behind.scm"
			"api/logout.scm"
		)
	)
	(provide program-command) ; Execute commands from game chat.

	; TODO parse(command) e.g. (parse "follow [me]|my|<name>") ; string -> verb, [noun], [noun...]
	; TODO get-targets(command) ; string -> list of object-id

	; (define (parse-targets connection author . arguments)
	; 	(define (parse-target token [default #f])
	; 		(case (or token default)
	; 			(("me") (@: (or author (list)) 'object-id))
	; 			(("self") (@: connection 'world 'me 'object-id))
	; 			(("my") (@: (or author (list)) 'target-id))
	; 			(("us" "party")
	; 				(displayln (@: connection 'world 'party))
	; 				(list) ; TODO
	; 			)
	; 			(else (and (string? token)
	; 				(ref (or (find-character-by-name (@: connection 'world) token) (list)) 'object-id)
	; 			))
	; 		)
	; 	)
	;
	; 	(if (not (null? arguments))
	; 		(fold (lambda (s p)
	; 			(let ((t (parse-target s)))
	; 				(append p (cond
	; 					((integer? t) (list t))
	; 					((not t) (list))
	; 					(else t)
	; 				))
	; 			)
	; 		) (list) arguments)
	; 		(list (parse-target #f))
	; 	)
	; )

	(define (parse-command wr text channel author-id)
		(if (or (eq? channel 'chat-channel/tell) (and (eq? channel 'chat-channel/party) (eq? author-id (party-leader (world-party wr)))))
			(remove (compose zero? string-length) (string-split (string-downcase text) " "))
			#f
		)
	)

	(define (command cn event config state)
		(when (eq? (car event) 'message)
			(let-values (((author-id channel author text) (apply values (cdr event))) ((brain) (car config)))
				(let* ((wr (connection-world cn)) (command (parse-command wr text channel author-id)))
					(when command (case (car command)
						(("chase") (brain-do! brain (program program-follow-chase author-id)))
						(("repeat") (brain-do! brain (program program-follow-repeat author-id)))
						(("relax") (brain-clear! brain #t))
						(("return") (let ((author (object-ref wr author-id)))
							; (when author (move-to cn (get-position author) (or (ref author 'collision-radius) 10)))
							(when author (move-behind cn author 50))
						))
						(("go") (let* ((on (or (string->number (list-try-ref command 1 "0")) 0)) (es (/ on (ref (world-me wr) 'run-speed))))
							(when (> on 0)
								(move-on cn on)
							)
						))
						(("hello") (gesture cn 'gesture/hello))
						(("bye")
							(brain-clear! brain #t) ; Call foreground program destructor before exit.
							(logout cn)
						)
						(else (say cn "I don't understand"))
					))
				)
			)
		)
		(void)
	)

	(define-program program-command
		(list
			undefined ; program manager (required)
		)
		undefined
		undefined
		command
	)
)
