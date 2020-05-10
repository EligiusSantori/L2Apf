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
	(provide program-bless)

	(define (program-error message . args)
		(apply raise-program-error 'program-bless message args)
	)

	(define (estimate character)
		(apply select-skills (append (cond
			((fighter-type? character) (list ; TODO Fill up.
				; Common.
				'might
				'shield
				'mental-shield
				'death-whisper
				'focus
				'guidance

				; Important.
				'vampiric-rage
				'wind-walk
			))
			((mystic-type? character) (cond
				((wizard-class? character) (list ; TODO Fill up.
					; Common.
					'shield
					'mental-shield
					'concentration

					; Important.
					'empower
				))
				((support-class? character) (list ; TODO Fill up.
					; Common.
					'shield
					'mental-shield

					; Important.
					'concentration
			))))) (list ; Global buffs.
			'wind-walk
		)))
	)

	(define (will-ready skill)
		(+ (ref skill 'last-usage) (ref skill 'reuse-delay))
	)

	(define (exists wr skill-ids)
		(fold (lambda (skill-id heap)
			(let ((skill (skill-ref wr skill-id)))
				(when (and skill (ref skill 'active?))
					(heap-add! heap skill)
				)
				heap
			)
		) (make-heap (lambda (a b)
			(<= (will-ready a) (will-ready b))
		)) skill-ids)
	)

	(define (next-buff cn todo)
		(if (> (heap-count todo) 0)
			(let* ((skill (heap-min todo)))
				(when (>= (timestamp) (will-ready skill))
					(use-skill cn (skill-id skill)) ; Buff is ready, do it.
				)
				todo
			)
			eof ; Everything completed.
		)
	)
	(define (buff-over cn todo)
		(let ((skill (heap-min todo))) ; Resort current.
			(heap-remove-min! todo)
			(heap-add! todo skill)
		)
		(next-buff cn todo) ; Do nearest.
	)

	(define-program program-bless
		(lambda (cn ev config todo)
			(let* ((target-id (car config)) (wr (connection-world cn)) (me (world-me wr)))
				(or (case-event ev
					('change-target (subject-id id . rest)
						(and (= (object-id me) subject-id) id (= id target-id)
							(next-buff cn todo) ; Start working.
						)
					)
					('skill-launched (subject-id . rest)
						(and (= (object-id me) subject-id)
							(begin
								(heap-remove-min! todo) ; Drop completed buff.
								(next-buff cn todo) ; Do next.
							)
						)
					)
					('skill-canceled (subject-id . rest) ; Sort queue then do nearest.
						(and (= (object-id me) subject-id)
							(buff-over cn todo)
						)
					)
					('skill-reusing (skill) ; Sort queue then do nearest.
						(and (> (heap-count todo) 0) (= (skill-id (heap-min todo)) (skill-id skill))
							(begin
								(buff-over cn todo)
							)
						)
					)
					('skill-reused (skill)
						(and (> (heap-count todo) 0) (= (skill-id (heap-min todo)) (skill-id skill))
							(next-buff cn todo) ; Next skill ready, do it.
						)
					)
					('die (subject-id . rest) ; Exit if me or target died.
						(if (member subject-id (list (object-id me) target-id)) eof todo)
					)
				) todo)
			)
		)

		#:constructor (lambda (cn config)
			(let-values (((target-id custom) (list->values config)))
				(let* ((wr (connection-world cn)) (me (world-me wr)) (whom (object-ref wr target-id)))
					(when (not whom) (program-error "Don't see the target." target-id))
					(when (not (creature? whom)) (program-error "Object is not creature." target-id))

					(let ((todo (exists wr (or custom (estimate whom)))))
						(if (> (heap-count todo) 0)
							(if (eq? (ref me 'target-id) target-id)
								(next-buff cn todo)
								(begin (target cn target-id) todo)
							)
							(program-error "Can do nothing for target." target-id)
						)
					)
				)
			)
		)

		#:defaults (list
			undefined ; target-id (required)
			#f ; custom buff list
		)
	)
)
