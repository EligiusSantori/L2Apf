(module ai racket/base
	(require
		srfi/1
		racket/undefined
		"program.scm"
		(relative-in "../."
			; "library/extension.scm"
			"system/structure.scm"
			"system/connection.scm"
			"system/event.scm"
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

	(define (slay/swordman cn wr me)
		(attack cn) ; TODO Skills.
	)
	;(define (slay/maceman ...) ...)
	;(define (slay/spearman ...) ...)
	;(define (slay/dualist ...) ...)
	;(define (slay/assassin ...) ...)
	;(define (slay/archer ...) ...)
	;(define (slay/wizard ...) ...)

	(define (slay/auto cn wr me)
		(slay/swordman cn wr me) ; TODO Resolve type.
	)

	(define (slay cn wr me victim-id)
		(let ((target-id (ref me 'target-id)))
			(if (and target-id (= victim-id target-id))
				(slay/auto cn wr me)
				(target cn victim-id)
			)
			(void)
		)
	)

	(define-program program-slay
		(lambda (cn ev config state)
			(let* ((victim-id (car config)) (wr (connection-world cn)) (me (world-me wr)))
				(case-event ev
					('change-target (subject-id target-id . rest)
						(when (and (= (object-id me) subject-id) target-id (= target-id victim-id))
							(slay cn wr me victim-id)
						)
					)
					(else (if (and (member (event-name ev) (list 'die 'object-delete) eq?) (= (second ev) victim-id))
						eof
						(void)
					))
				)
			)
		)

		#:constructor (lambda (cn config)
			(let* ((victim-id (car config)) (wr (connection-world cn)) (victim (object-ref wr victim-id)))
				(when (not victim) (program-error "Don't see the target." victim-id))
				(when (not (creature? victim)) (program-error "Object is not creature." victim-id))
				(when (protagonist? victim) (program-error "Can't attack myself." victim-id))

				(slay cn wr (world-me wr) victim-id)
			)
		)

		#:defaults (list
			undefined ; victim-id (required)
		)
	)
)
