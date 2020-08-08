(module ai racket/base
	(require
		srfi/1
		racket/undefined
		data/heap
		"program.scm"
		(relative-in "../."
			"library/extension.scm"
			"library/date_time.scm"
			"system/structure.scm"
			"system/connection.scm"
			"system/event.scm"
			"model/skill.scm"
			"model/object.scm"
			"model/creature.scm"
			"model/character.scm"
			"model/world.scm"
			"api/target.scm"
			"api/use_skill.scm"
		)
	)
	(provide make-program-bless)

	(define (program-error message . args)
		(apply raise-program-error 'program-bless message args)
	)

	(define (will-ready skill)
		(+ (ref skill 'last-usage) (ref skill 'reuse-delay))
	)

	(define (get-todo cn estimate character-id)
		(let* ((wr (connection-world cn)) (character (object-ref wr character-id)))
			(when (not character) (program-error "Don't see the target." character-id))
			(when (not (character? character)) (program-error "Object is not a character." character-id))
			(fold (lambda (name heap)
				(let ((skill (find-skill wr name)))
					(when skill (heap-add! heap skill))
					heap
				)
			) (make-heap (lambda (a b)
				(<= (will-ready a) (will-ready b))
			)) (estimate character))
		)
	)

	(define (next-buff cn todo)
		(let ((skill (heap-min todo)))
			(when (>= (timestamp) (will-ready skill))
				(use-skill cn skill) ; Buff is ready, do it.
			)
		)
	)
	(define (buff-over cn todo)
		(let ((skill (heap-min todo))) ; Resort current.
			(heap-remove-min! todo)
			(heap-add! todo skill)
		)
		(next-buff cn todo) ; Do nearest.
	)
	(define (do-buff cn estimate target-ids todo)
		(if (> (heap-count todo) 0)
			(begin
				(if (eq? (ref (world-me (connection-world cn)) 'target-id) (car target-ids))
					(next-buff cn todo)
					(target cn (car target-ids))
				)
				(cons target-ids todo)
			)
			(if (not (null? (cdr target-ids)))
				(do-buff cn estimate (cdr target-ids) (get-todo cn estimate (cadr target-ids)))
				eof ; Everything completed.
			)
		)
	)

	(define (make-program-bless target-ids estimate)
		(make-program 'program-bless
			(lambda (cn ev state)
				(let-values (((target-ids todo) (car+cdr state)))
					(let* ((wr (connection-world cn)) (me (world-me wr)))
						(or (case-event ev
							('change-target (subject-id target-id . rest)
								(and (= (object-id me) subject-id) target-id (= target-id (car target-ids))
									(begin (next-buff cn todo) state) ; Start working.
								)
							)
							('skill-launched (subject-id . rest)
								(and (= (object-id me) subject-id)
									(begin
										(heap-remove-min! todo) ; Drop completed buff.
										(do-buff cn estimate target-ids todo) ; Do next.
									)
								)
							)
							('skill-canceled (subject-id . rest) ; Sort queue then do nearest.
								(and (= (object-id me) subject-id)
									(begin (buff-over cn todo) state)
								)
							)
							('skill-reusing (skill) ; Sort queue then do nearest.
								(and (> (heap-count todo) 0) (= (skill-id (heap-min todo)) (skill-id skill))
									(begin (buff-over cn todo) state)
								)
							)
							('skill-reused (skill)
								(and (> (heap-count todo) 0) (= (skill-id (heap-min todo)) (skill-id skill))
									(begin (next-buff cn todo) state) ; Next skill ready, do it.
								)
							)
							('die (subject-id . rest) ; Exit on die.
								(if (= (object-id me) subject-id) eof state)
							)
						) state)
					)
				)
			)

			#:constructor (lambda (cn)
				(when (null? target-ids) (program-error "Nothing to do."))
				(let ((state (do-buff cn estimate target-ids (get-todo cn estimate (car target-ids)))))
					(when (eof-object? state) (program-error "Nothing to do."))
					state
				)
			)
		)
	)
)
