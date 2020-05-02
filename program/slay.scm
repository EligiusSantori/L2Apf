(module ai racket/base
	(require
		srfi/1
		; racket/math
		racket/undefined
		"program.scm"
		(relative-in "../."
			"library/extension.scm"
			"system/structure.scm"
			"system/connection.scm"
			"system/event.scm"
			; "model/map.scm"
			"model/object.scm"
			"model/creature.scm"
			"model/protagonist.scm"
			"model/world.scm"
			"api/target.scm"
			"api/attack.scm"
		)
	)
	(provide program-slay)

	(define (program-error message . args)
		(apply raise-program-error 'program-slay message args)
	)

	(define (slay cn wr me victim-id)
		(let ((target-id (ref me 'target-id)))
			(if (and target-id (= victim-id target-id))
				(attack cn)
				(target cn victim-id)
			)
(if (and target-id (= victim-id target-id))
	(printf "{slay attack}~n")
	(printf "{slay target}~n")
)
			(void)
		)
	)

	(define-program program-slay
		(list
			undefined ; target-id (required)
		)
		(lambda (cn config)
			(let-values (((target-id) (list->values config)))
				(let* ((wr (connection-world cn)) (trg (object-ref wr target-id)))
					(when (not trg) (program-error "Don't see the target." target-id))
					(when (not (creature? trg)) (program-error "Target is not creature." target-id))
					(when (protagonist? trg) (program-error "Can't attack myself." target-id))

					(slay cn wr (world-me wr) target-id)
				)
			)
		)
		undefined
		(lambda (cn ev config state)
			(let-values (((victim-id) (list->values config)))
				(let* ((wr (connection-world cn)) (me (world-me wr)))
					(case-event ev
						(change-target (subject-id target-id)
							(when (and (= (object-id me) subject-id) target-id (= target-id victim-id))
								(slay cn wr me victim-id)
							)
						)
						(else (if (and (member (car ev) (list 'die 'object-delete) eq?) (= (second ev) victim-id))
							eof
							(void)
						))
					)
				)
			)
		)
	)
)
