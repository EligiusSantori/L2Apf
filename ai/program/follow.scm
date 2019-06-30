(module ai racket/base
	(require
		; srfi/1
		racket/undefined
		data/queue
		"program.scm"
		(relative-in "../../.."
			"library/structure.scm"
			"library/extension.scm"
			"model/map.scm"
			"model/creature.scm"
			"model/world.scm"
			"api/move_to.scm"
"api/say.scm"
		)
	)
	(provide
		ai-program-follow
	)

	(define queue undefined) ; TODO move to state
	(define last #f) ; TODO move to state

	(define (follow event connection config state) (let-values (((leader-id manner margin) (list->values config)))
		(let* ((world (ref connection 'world)) (me (ref world 'me)) (leader (object-ref world leader-id)))
			(define (gap)
				(+ margin (or (ref leader 'collision-radius) 0))
			)
			(define (go point)
				(and point (move-to connection point (gap)))
			)
			(define (add! point)
				(if (and point (> (points-distance (or last (get-position me)) point) (gap))) ; If point makes sense.
					(begin
						(when last (enqueue! queue last))
						(set! last point)
						#t
					)
					#f
				)
			)
			(define (next!)
;(define n
				(case manner
					((chase) (or (ref leader 'destination) (get-position leader)))
					((repeat) (or
						(if (not (queue-empty? queue)) (dequeue! queue) #f)
						(if last (let ((p last)) (set! last #f) p) #f)
						(ref leader 'destination)
						(get-position leader)
					))
					; ((period)
					; 	; TODO
					; )
				)
;) (say connection (format "Going on ~a." (if n (points-distance (get-position me) n) #f))) n
			)

			(if leader ; TODO else throw error and display to console or chat
				(begin
					(case (if event (car event) #f)
						((change-moving) (let-values (((object-id position destination) (apply values (cdr event))))
							(when (eq? object-id leader-id) (case manner
								((chase) (go (next!)))
								((repeat) (let ((to (ref leader 'destination)) (at (get-position leader)))
									(if (not last) ; Is moving to leader destination or standing
										(or (go at) (go to)) ; Go to leader position or destination
										(add! at) ; Enqueue leader position
									)
								))
							))
						))
					)

					(when (not (moving? me))
						(go (next!))
					)
				)
				(displayln "I don't see the leader." (current-error-port))
			)
		)

		(void)
	))

	(define ai-program-follow (struct-copy ai-program ai-program-base
		[id 'follow]
		[constructor (lambda (config) (let-values (((leader-id manner margin) (list->values config)))
				(set! queue (if (eq? manner 'repeat) (make-queue) undefined))
				(set! last #f)
				(void)
		))]
		[iterator follow]
		[config (list
			undefined ; leader-id (required)
			'chase ; manner {chase, repeat, period}
			30 ; margin
		)]
	))
)
