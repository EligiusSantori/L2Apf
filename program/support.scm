(module ai racket/base
	(require
		(only-in srfi/1 fold car+cdr)
		racket/undefined
		"program.scm"
		(relative-in "../."
			"library/extension.scm"
			"system/structure.scm"
			"system/connection.scm"
			"system/event.scm"
			"model/map.scm"
			"model/skill.scm"
			"model/item.scm"
			"model/object.scm"
			"model/creature.scm"
			"model/character.scm"
			"model/party.scm"
			"model/world.scm"
			"api/target.scm"
			"api/use_skill.scm"
			"api/logout.scm"
		)
	)
	(provide program-support)

	(define (program-error message . args)
		(apply raise-program-error 'program-support message args)
	)
	(define (error-dead)
		(program-error "Must be alive.")
	)
	(define (error-no-party)
		(program-error "Not in party.")
	)

	(define (member-dead? character)
		(and
			(ref character 'alike-dead?)
			(<= (ref character 'hp) 0)
		)
	)
	(define (anybody-alive? wr ids) ; Fake dead consider dead.
		(fold (lambda (id is)
			(or is
				(let ((creature (object-ref wr id)))
					(and creature (alive? creature))
				)
			)
		) #f ids)
	)
	(define (should-heal? creature heal?)
		(let ((ratio (hp-ratio creature)))
			(and
				heal?
				(not (member-dead? creature))
				(if (procedure? heal?)
					(heal? creature)
					(<= ratio 2/3)
				)
				ratio
			)
		)
	)
	(define (hp-danger? ratio)
		(<= ratio 1/5)
	)
	(define (should-recharge? character recharge?)
		(let ((ratio (mp-ratio character)))
			(and
				recharge?
				(not (member-dead? character))
				(not (recharger-class? character)) ; Meaningless to recharge recharger.
				(if (procedure? recharge?)
					(recharge? character)
					(<= ratio 1/3)
				)
				ratio
			)
		)
	)
	(define (should-resurrect? character resurrect?)
		(and
			resurrect?
			(member-dead? character)
		)
	)

	(define (filter-skills wr skill-ids)
		(reverse (fold (lambda (skill-id r)
			(let ((skill (skill-ref wr skill-id)))
				(if skill (cons skill r) r)
			)
		) (list) skill-ids))
	)
	(define (find-heal-skill wr fast? party?)
		(define heal-grp (list 'greater-heal 'heal))
		(define fast-grp (list 'greater-battle-heal 'battle-heal))
		(define party-grp (list 'greater-group-help 'group-heal))

		(or (try-first (filter-skills wr (apply select-skills (cond
			((and fast? party?) (append party-grp fast-grp heal-grp))
			(party? (append party-grp heal-grp fast-grp))
			(fast? (append fast-grp heal-grp party-grp))
			(else (append heal-grp fast-grp party-grp))
		)))) (program-error "Can't find heal skill."))
	)
	(define (find-recharge-skill wr)
		(or
			(try-first (filter-skills wr (select-skills 'recharge)))
			(program-error "Can't find recharge skill.")
		)
	)
	(define (find-resurrect-skill wr)
		(or
			(try-first (filter-skills wr (select-skills 'resurrection 'mass-resurrection)))
			(program-error "Can't find resurrection skill.")
		)
	)

	(define (fix-target cn me to) ; False if target ok, true if fixing.
		(if (eq? (ref me 'target-id) (object-id to))
			#f (begin (target cn (object-id to)) #t)
		)
	)
	(define (check cn wr me party range heal? recharge? resurrect? [memeber-id #f])
		(define (check-all member-ids)
			(let-values (((member-id rest) (car+cdr member-ids)))
				(or ; Action taken or check next member.
					(let ((character (object-ref wr member-id)))
						(and character (or (zero? range) (<= (creatures-distance me character) range)) ; If member is not too far.
							(cond
								((should-heal? character heal?)
									(or
										(fix-target cn me character)
										(use-skill cn (skill-id (find-heal-skill wr (hp-danger? (hp-ratio character)) #f)))
									)
								)
								((should-recharge? character recharge?)
									(or
										(fix-target cn me character)
										(use-skill cn (skill-id (find-recharge-skill wr)))
									)
								)
								((should-resurrect? character resurrect?)
									(or
										(fix-target cn me character)
										(use-skill cn (skill-id (find-resurrect-skill wr)))
									)
								)
								(else #f)
							)
						)
					)
					(and (not (null? rest))
						(check-all rest)
					)
				)
			)
		)

		(and (not (casting? me))
			(check-all (if memeber-id (list memeber-id) (party-members party)))
		)
	)

	(define-program program-support
		(lambda (cn event config state)
			(let-values (((heal? recharge? resurrect? range) (list->values config)))
				(let* ((wr (connection-world cn)) (me (world-me wr)) (party (world-party wr))
							(do-check (bind-head check cn wr me party range heal? recharge? resurrect?)))
					(case-event event
						('change-target (subject-id target-id . rest)
							(when (and (= subject-id (object-id me)) target-id (in-party? party target-id))
								(do-check target-id)
							)
						)
						('creature-update (creature-id changes)
							(when (in-party? party creature-id)
								(do-check creature-id)
							)
						)
						('skill-launched (subject-id . rest)
							(when (= subject-id (object-id me))
								(do-check)
							)
						)
						('skill-canceled (subject-id . rest)
							(when (= subject-id (object-id me))
								(do-check (ref me 'target-id))
							)
						)
						('skill-reusing (skill)
							(do-check (ref me 'target-id))
						)
						('skill-reused (skill)
							(do-check)
						)
						('die (subject-id . rest)
							(cond
								((= subject-id (object-id me)) (error-dead))
								((in-party? party subject-id)
									(if resurrect?
										(do-check subject-id)
										(when (not (anybody-alive? wr (party-members party)))
											(logout cn)
											(program-error "Everyone dead.")
										)
									)
								)
							)
						)
						('party-memeber-join (subject-id)
							(do-check subject-id)
						)
						('party-leave ()
							(when (not party) (error-no-party))
						)
					)
					(void)
				)
			)
		)

		#:constructor (lambda (cn config)
			(let-values (((heal? recharge? resurrect? range) (list->values config)))
				(let* ((wr (connection-world cn)) (me (world-me wr)) (party (world-party wr)))
					(when (not party) (error-no-party))
					(when (ref me 'dead?) (error-dead))

					(check cn wr me party range heal? recharge? resurrect?)
					(void)
				)
			)
		)

		#:defaults (list
			#t ; heal?
			#t ; recharge?
			#f ; resurrect?
			0 ; max distance
		)
	)
)
