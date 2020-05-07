(module ai racket/base
	(require
		(only-in srfi/1 remove)
		racket/string
		racket/undefined
		"program.scm"
		"follow_chase.scm"
		"follow_repeat.scm"
		"slay.scm"
		"bless.scm"
		"support.scm"
		"pickup.scm"
		"relax.scm"
		"brain.scm"
		(relative-in "../."
			"library/extension.scm"
			"system/structure.scm"
			"system/connection.scm"
			"system/event.scm"
			"model/object.scm"
			"model/item.scm"
			"model/creature.scm"
			"model/npc.scm"
			"model/party.scm"
			"model/map.scm"
			"model/world.scm"
			"api/say.scm"
			"api/gesture.scm"
			"api/move_to.scm"
			"api/move_on.scm"
			"api/move_behind.scm"
			"api/target.scm"
			"api/attack.scm"
			"api/use_skill.scm"
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
		(define brain (car config))
		(case-event event
			('message (author-id channel author text)
				(let* ((wr (connection-world cn)) (me (world-me wr)) (command (parse-command wr text channel author-id)))
					(when command (case (car command)
						; Actions.
						(("hello") (gesture cn 'gesture/hello))
						(("return") (let ((author (object-ref wr author-id)))
							; (when author (move-to cn (get-position author) (or (ref author 'collision-radius) 10)))
							(when author (move-behind cn author 50))
						))
						; TODO
						; (("use") (let* ((what (list-try-ref command 1)) (item-id (string->number (or what "")) 0)) (case what
						; 	(("soe") (fold-items wr (list) (lambda (item lst)
						; 		(if (member (ref item 'item-id) (list
						; 			736 1829 1830  ; Normal.
						; 			1538 5858 5859 3958 ; Blessed.
						; 			7117 7118 7119 7120 7121 ; Villages.
						; 			7122 7123 7124 7125 7126 7128 7129 7131 7132 7133 7134 7135 ; Towns.
						; 		)) (cons (ref item 'item-id) (ref item 'object-id)) lst)
						; 	))
						; 	(else (when (> item-id 0) (use-item cn item-id)))
						; )))
						(("show") (let ((what (list-try-ref command 1))) (case what
							(("level") (say cn (format "Level: ~a." (ref me 'level)) 'chat-channel/party))
							(("sp") (say cn (format "SP: ~a." (ref me 'sp)) 'chat-channel/party))
							; (("adena") ...) ; TODO
						)))
						; (("go") (let* ((on (or (string->number (list-try-ref command 1 "0")) 0)) (es (/ on (ref (world-me wr) 'run-speed))))
						; 	(when (> on 0)
						; 		(move-on cn on)
						; 	)
						; ))
						(("bye")
							(brain-clear! brain #t) ; Call foreground program destructor before exit.
							(logout cn)
						)

						; Programs.
						(("follow") (let ((gap (or (string->number (list-try-ref command 1 "30")) 30)))
							(brain-do! brain (program program-follow-chase author-id gap))
						))
						(("follow+") (brain-do! brain (program program-follow-repeat author-id 50)))

						(("assist") (let ((author (object-ref wr author-id)))
							(if author
								(brain-do! brain (program program-slay (ref author 'target-id)) #t)
								(say cn "Don't see the requester.")
							)
						))
						(("support")
							(brain-load! brain (program program-support #t #t #f #t 1000))
							(brain-do! brain (program program-follow-chase author-id 150))
						)
						(("buff") (let ((author (object-ref wr author-id)))
							(if author
								(brain-do! brain (program program-bless (ref author 'target-id)) #t)
								(say cn "Don't see the requester.")
							)
						))
						(("pickup") (let ((position (get-position (world-me wr))))
							(define (find-closest object closest)
								(if (and (item? object) (on-ground? object))
									(let ((d (points-distance (ref object 'position) position)))
										(if (or (not closest) (< d (car closest)))
											(cons d object)
											closest
										)
									)
									closest
								)
							)

							(let ((closest (fold-objects wr #f find-closest)))
								(if closest
									(brain-do! brain (program program-pickup (object-id (cdr closest))) #t)
									(say cn "Don't see an item.")
								)
							)
						))
						(("relax") (let ((duration (or (string->number (list-try-ref command 1 "0")) 0)))
							(brain-do! brain (program program-relax duration))
						))
						(("clear") (brain-clear! brain #t))

						(else (say cn "I don't understand."))
					))
				)
			)
		)
		(void)
	)

	(define-program program-command
		command

		#:defaults (list
			undefined ; program manager (required)
		)
	)
)
