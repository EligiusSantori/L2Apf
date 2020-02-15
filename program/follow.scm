(module ai racket/base
	(require
		srfi/1
		racket/undefined
		data/queue
		"program.scm"
		(relative-in "../.."
			"library/extension.scm"
			"system/structure.scm"
			"model/map.scm"
			"model/creature.scm"
			"model/world.scm"
			"api/move_to.scm"
		)
	)
	(provide
		program-follow
	)

	(define (follow event connection config state)
		(let-values (((leader-id manner margin) (list->values config)) ((now? queue last) (list->values state)))
			(let* ((world (ref connection 'world)) (me (ref world 'me)) (leader (object-ref world leader-id)))
				(define (gap)
					(+ margin (or (ref leader 'collision-radius) 0))
				)
				(define (go point)
(define p
					(and point (move-to connection point (gap)))
) (when p (printf "Going on ~a queue ~a.~n~e~n" (points-distance p (get-position me)) (queue-length queue) event) (flush-output (current-output-port)))

				)
				(define (far-enough? point [from #f])
					(> (points-distance point (or from (get-position me))) (gap))
				)
				(define (add! point)
					(if (and point (far-enough? point last)) ; Skip if interval is too small.
						(begin
							(when last (enqueue! queue last))
							(set! last point)
							#t
						)
						#f
					)
				)
				(define (next!)
					(case manner
						((chase) (or (ref leader 'destination) (get-position leader)))
						((repeat) (or
							(if (not (queue-empty? queue)) (dequeue! queue) #f)
							(if last (let ((p last)) (set! last #f) p) #f)
							(let ((p (get-position leader)))
								(if (far-enough? p) p #f)
							)
							(ref leader 'destination)
						))
						; ((period)
						; 	; TODO
						; )
					)
				)
				(when (or now? (eq? (car event) 'change-moving))
					(let ((moving-id (second event)))
						(if leader ; TODO else throw error and display to console or chat
							(begin
								(cond
									((and (eq? manner 'chase) (or now? (= moving-id leader-id)))
										(go (next!))
									)
									((and (eq? manner 'repeat) (or now? (member moving-id (list leader-id (ref me 'object-id)) =)))
										(when (and last (= moving-id leader-id) ) ; If abide the route.
											(add! (get-position leader)) ; Add next point.
										)
										(when (not (moving? me))
											(go (next!))
										)
									)
								)

								(list #f queue last)
							)
							(begin
								(displayln "I don't see the leader." (current-error-port))
								state
							)
						)
					)
				)
			)
		)
	)

	(define-program program-follow
		(list
			undefined ; leader-id (required)
			'chase ; manner {chase, repeat, period}
			30 ; margin
		)
		(lambda (config)
			(if (eq? (second config) 'repeat)
				(list #t (make-queue) #f)
				(list #t undefined undefined)
			)
		)
		undefined
		follow
	)
)
