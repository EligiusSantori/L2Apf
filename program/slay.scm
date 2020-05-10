(module ai racket/base
	(require
		(only-in srfi/1 fold)
		racket/undefined
		"program.scm"
		(relative-in "../."
			"library/extension.scm"
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

	(define (mp>2/3? me . rest)
		(> (mp-ratio me) 2/3)
	)
	(define (mp>1/3? me . rest)
		(> (mp-ratio me) 1/3)
	)
	(define (target-hp>2/3? me target . rest)
		(> (hp-ratio target) 2/3)
	)
	(define (target-hp>1/3? me target . rest)
		(> (hp-ratio target) 1/3)
	)
	(define (equip-sword? . rest)
		#t ; TODO
	)
	(define (equip-blunt? . rest)
		#t ; TODO
	)
	(define (equip-duals? . rest)
		#t ; TODO
	)
	(define (equip-spear? . rest)
		#t ; TODO
	)
	(define (equip-dagger? . rest)
		#t ; TODO
	)
	(define (equip-bow? . rest)
		#t ; TODO
	)

	(define (transform conditions)
		(let ((skill-ids (apply select-skills (map car conditions))))
			(map (lambda (skill-id pair) (cons skill-id (cdr pair))) skill-ids conditions)
		)
	)
	(define conditions (transform (list
		(cons 'power-strike (list equip-sword? mp>1/3? target-hp>1/3?))
		(cons 'mortal-blow (list equip-dagger? mp>1/3? target-hp>2/3?))
		(cons 'power-shot (list equip-bow? mp>2/3? target-hp>1/3?))
		(cons 'spoil (lambda (me target . rest)
			(and
				(npc? target)
				(not (ref target 'spoiled?))
				(> (mp-ratio me) 1/5)
			)
		))
	)))
	(define (should-skill? me target skill)
		(and skill (skill-ready? skill) (>= (ref me 'mp) (or (ref skill 'mp-cost) 0))
			(let ((conditions (alist-ref conditions (skill-id skill) always?)))
				(not (fold (lambda (condition failed)
					(or failed (not (condition me target skill)))
				) #f (if (procedure? conditions) (list conditions) conditions)))
			)
			skill
		)
	)
	(define (find-skill wr me skill-ids) ; First fit.
		(let ((target (get-target wr me)))
			(fold (lambda (skill-id found)
				(or found (should-skill? me target (skill-ref wr skill-id)))
			) #f skill-ids)
		)
	)
	(define (exists-skills wr names)
		(fold (lambda (skill-id name r)
			(let ((skill (skill-ref wr skill-id)))
				(if skill (cons skill-id r) r)
			)
		) (list) (apply select-skills names) names)
	)

	;(define (slay/soldier ...) ...)
	;(define (slay/assassin ...) ...)
	;(define (slay/archer ...) ...)
	;(define (slay/wizard ...) ...)

	(define (slay/auto cn wr me skill-ids)
		(let ((skill (find-skill wr me skill-ids)))
			(if skill
				(use-skill cn (skill-id skill))
				(attack cn)
			)
		)
	)

	(define (slay cn wr me victim-id skills)
		(let ((target-id (ref me 'target-id)))
			(if (and target-id (= victim-id target-id))
				(slay/auto cn wr me skills)
				(target cn victim-id)
			)
		)
		(void)
	)

	(define-program program-slay ; TODO Auto-resolve default skills.
		(lambda (cn event config skill-ids)
			(let-values (((victim-id skills) (list->values config)))
				(let* ((wr (connection-world cn)) (me (world-me wr)) (do-slay (bind-head slay cn wr me victim-id skill-ids)))
					(if (case-event event
						('change-target (subject-id target-id . rest)
							(when (= (object-id me) subject-id)
								(do-slay)
							)
						)
						('skill-launched (subject-id skill . rest)
							(when (and (= (object-id me) subject-id) (member (skill-id skill) skill-ids =))
								(do-slay)
							)
						)
						('skill-canceled (subject-id skill)
							(when (and (= (object-id me) subject-id) (member (skill-id skill) skill-ids =))
								(do-slay)
							)
						)
						('skill-reusing (skill) (when (member (skill-id skill) skill-ids =) (do-slay)))
						('skill-reused (skill) (when (member (skill-id skill) skill-ids =) (do-slay)))
						('object-delete (id) (not (= id victim-id)))
						('die (id . rest) (not (member id (list (object-id me) victim-id) =)))
						(else (void))
					) skill-ids eof)
				)
			)
		)

		#:constructor (lambda (cn config)
			(let-values (((victim-id skills) (list->values config)))
				(let* ((wr (connection-world cn)) (victim (object-ref wr victim-id)))
					(when (not victim) (program-error "Don't see the target." victim-id))
					(when (not (creature? victim)) (program-error "Object is not creature." victim-id))
					(when (protagonist? victim) (program-error "Can't attack myself." victim-id))

					(let ((skill-ids (exists-skills wr skills)))
						(slay cn wr (world-me wr) victim-id skill-ids)
						skill-ids
					)
				)
			)
		)

		#:defaults (list
			undefined ; victim-id (required)
			(list) ; skills
		)
	)
)
