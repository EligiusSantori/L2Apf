(module ai racket/base
	(require
		(only-in srfi/1 fold)
		racket/undefined
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
			"model/npc.scm"
			"model/protagonist.scm"
			"model/world.scm"
			"api/target.scm"
			"api/attack.scm"
			"api/use_skill.scm"
		)
	)
	(provide program-slay)

	(define (program-error message . args)
		(apply raise-program-error 'program-slay message args)
	)

	(define (should-skill? me target skill check)
		(and
			(skill-ready? skill)
			(>= (ref me 'mp) (or (ref skill 'mp-cost) 0))
			(check me target skill)
			skill
		)
	)
	(define (choose-skill wr me skills) ; First fit.
		(let ((target (get-target wr me)))
			(fold (lambda (p found)
				(or found (should-skill? me target (skill-ref wr (car p)) (cdr p)))
			) #f skills)
		)
	)
	(define (parse-skills wr skills)
		(fold (lambda (p r)
			(let ((skill (find-skill wr (car p))))
				(if skill (cons (cons (skill-id skill) (cdr p)) r) r)
			)
		) (list) skills)
	)

	;(define (slay/soldier ...) ...)
	;(define (slay/assassin ...) ...)
	;(define (slay/archer ...) ...)
	;(define (slay/wizard ...) ...)
	;(define (slay/auto ...) ...)

	(define (slay cn wr me victim-id skills action)
		(let ((target-id (ref me 'target-id)))
			(if (and target-id (= victim-id target-id))
				(when (not (ref me 'casting))
					(let ((skill (choose-skill wr me skills)))
						(cond
							(skill (use-skill cn skill #t))
							(action (action me (get-target wr me)))
							(else (attack cn))
						)
					)
				)
				(target cn victim-id)
			)
		)
		(void)
	)

	(define on-changes (list 'hp 'bleeding? 'poisoned? 'burning? 'stunned? 'silenced?))

	(define-program program-slay
		(lambda (cn event config skills)
			(let-values (((victim-id _ action) (list->values config)))
				(let* ((wr (connection-world cn)) (me (world-me wr)) (do-slay (bind-head slay cn wr me victim-id skills action)))
					(if (case-event event
						('change-target (subject-id target-id . rest)
							(when (member subject-id (list (object-id me) victim-id) =)
								(do-slay)
							)
						)
						('creature-update (subject-id changes)
							(and
								(member subject-id (list (object-id me) victim-id) =)
								(or
									(fold (lambda (c r) (or r (member (car c) on-changes eq?))) #f changes)
									(and (not (= subject-id victim-id)) (assoc 'mp changes eq?))
								)
								(do-slay)
							)
						)
						('skill-launched (subject-id skill . rest)
							(when (and (= (object-id me) subject-id) (assoc (skill-id skill) skills =))
								(do-slay)
							)
						)
						('skill-canceled (subject-id skill)
							(when (and (= (object-id me) subject-id) (assoc (skill-id skill) skills =))
								(do-slay)
							)
						)
						('skill-reusing (skill) (when (assoc (skill-id skill) skills =) (do-slay)))
						('skill-reused (skill) (when (assoc (skill-id skill) skills =) (do-slay)))
						('object-delete (id) (not (= id victim-id)))
						('die (id . rest) (not (member id (list (object-id me) victim-id) =)))
						(else (void))
					) skills eof)
				)
			)
		)

		#:constructor (lambda (cn config)
			(let-values (((victim-id skills action) (list->values config)))
				(let* ((wr (connection-world cn)) (victim (object-ref wr victim-id)))
					(when (not victim) (program-error "Don't see the target." victim-id))
					(when (not (creature? victim)) (program-error "Object is not creature." victim-id))
					(when (protagonist? victim) (program-error "Can't attack myself." victim-id))

					(let ((skills (parse-skills wr skills)))
						(slay cn wr (world-me wr) victim-id skills action)
						skills
					)
				)
			)
		)

		#:defaults (list
			undefined ; victim-id (required)
			(list) ; skills (alist)
			#f ; default action
		)
	)
)
