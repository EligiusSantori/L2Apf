(module ai racket/base
	(require
		(only-in srfi/1 fold)
		"program.scm"
		(relative-in "../."
			"library/extension.scm"
			"system/structure.scm"
			"system/connection.scm"
			"system/event.scm"
			"model/object.scm"
			"model/creature.scm"
			"model/character.scm"
			"model/skill.scm"
			"model/party.scm"
			"model/world.scm"
			"api/sit.scm"
			"api/target.scm"
			"api/use_skill.scm"
		)
	)
	(provide make-program-support)

	(define (program-error message . args) (apply raise-program-error 'program-support message args))
	(define (error-no-party) (program-error "Not in party."))

	(define (get-wounded wr min-ratio distance)
		(let ((me (world-me wr)))
			(sort
				(fold (lambda (id wounded)
					(let ((character (object-ref wr id)))
						(if (and character
								(not (ref character 'alike-dead?))
								(<= (hp-ratio character) min-ratio)
								(<= (creatures-distance character me) distance))
							(cons character wounded)
							wounded
						)
					)
				) (list) (party-members (world-party wr)))
				(lambda (a b) (let ((av (hp-ratio a)) (bv (hp-ratio b)))
					(if (= av bv) (< (object-id a) (object-id b)) (< av bv))
				))
			)
		)
	)
	(define (get-tired wr min-ratio distance)
		(let ((me (world-me wr)))
			(sort
				(fold (lambda (id tired)
					(let ((character (object-ref wr id)))
						(if (and character
								(not (ref character 'alike-dead?))
								(<= (mp-ratio character) min-ratio)
								(<= (creatures-distance character me) distance))
							(cons character tired)
							tired
						)
					)
				) (list) (party-members (world-party wr)))
				(lambda (a b) (let ((av (mp-ratio a)) (bv (mp-ratio b)))
					(if (= av bv) (< (object-id a) (object-id b)) (< av bv))
				))
			)
		)
	)
	(define (get-dead wr resurrect? distance)
		(let ((me (world-me wr)))
			(sort
				(fold (lambda (id dead)
					(let ((character (object-ref wr id)))
						(if (and character
								(ref character 'alike-dead?)
								(<= (creatures-distance character me) distance)
								(if (procedure? resurrect?) (resurrect? character) #t))
							(cons character dead)
							dead
						)
					)
				) (list) (party-members (world-party wr)))
				(lambda (a b) (< (object-id a) (object-id b))) ; TODO First tanks then supports then others.
			)
		)
	)

	(define (find-skills wr . names)
		(list->values (map (lambda (id) (find-skill wr id)) names))
	)
	(define (has-skills wr . names)
		(> (length (values->list (apply find-skills wr names))) 0)
	)
	(define (all-heals wr)
		(values
			(let-values (((hs ghs) (find-skills wr 'heal 'greater-heal #|'major-heal|#)))
				(or ghs hs) ; TODO Check spirit ore count.
			)
			(let-values (((bhs gbhs) (find-skills wr 'battle-heal 'greater-battle-heal)))
				(or gbhs bhs)
			)
			(let-values (((ghs gghs) (find-skills wr 'group-heal 'greater-group-heal #|'major-group-heal|#)))
				(or gghs ghs) ; TODO Check spirit ore count.
			)
		)
	)
	(define (skill-heal wr me wounded hp-steady hp-danger)
		(let-values (((simple-heal urgent-heal party-heal) (all-heals wr))) (cond
			((and hp-danger (<= (hp-ratio (car wounded)) hp-danger))
				(cond
					((and urgent-heal (skill-ready? urgent-heal)) urgent-heal)
					((and simple-heal (skill-ready? simple-heal)) simple-heal)
					(else #f) ; Wait because group heal is too slow.
				)
			)
			((and party-heal (> (length wounded) 2) (skill-ready? party-heal)) party-heal)
			((and simple-heal (skill-ready? simple-heal)) simple-heal)
			(else #f)
		))
	)
	(define (skill-recharge wr me tired)
		(let ((rs (find-skill wr 'recharge)))
			(and rs (skill-ready? rs) rs)
		)
	)
	(define (skill-resurrect wr me dead)
		(let-values (((rs crs) (find-skills wr 'resurrection 'mass-resurrection)))
			(cond
				((and (> (length dead) 1) crs (skill-ready? crs)) crs) ; TODO Filter dead by clan.
				((and rs (skill-ready? rs)) rs)
				(else #f)
			)
		)
	)
	(define (do-treatment cn me to skill)
		(cond
			((not (standing? me)) (sit cn #f))
			((not (eq? (ref me 'target-id) (object-id to)))
				(target cn (object-id to))
			)
			(else (use-skill cn skill))
		)
	)
	(define (check-and-treat cn hp-steady hp-danger mp-steady mp-reserve economy resurrect? distance)
		(let* ((wr (connection-world cn)) (me (world-me wr)))
			(and (not (casting? me)) (or
				(and (or hp-steady hp-danger) (let ((wounded (get-wounded wr (or hp-steady hp-danger) distance)))
					(and (not (null? wounded)) (let ((target (car wounded)) (skill (skill-heal wr me wounded hp-steady hp-danger)))
						(and (or (not economy) (<= (hp-ratio target) economy)) (do-treatment cn me target skill))
					))
				))
				(and mp-steady (or (not mp-reserve) (> (mp-ratio me) mp-reserve)) (let ((tired (get-tired wr mp-steady distance)))
					(and (not (null? tired)) (let ((target (car tired)) (skill (skill-recharge wr me tired)))
						(and (not (support-class? target)) (or (not economy) (<= (mp-ratio target) economy)) (do-treatment cn me target skill))
					))
				))
				(and resurrect? (let ((dead (get-dead wr resurrect? distance)))
					(and (not (null? dead)) (do-treatment cn me (car dead) (skill-resurrect wr me dead)))
				))
				(and economy (standing? me) (< (mp-ratio me) 97/100) ; Standby.
					(sit cn #t)
				)
			))
		)
	)

	(define (make-program-support [hp-steady 9/10] [hp-danger 1/5] [mp-steady 2/3] [mp-reserve 1/3] [economy 1/3] [resurrect? #f] [distance 1000])
		(make-program 'program-support
			(lambda (cn event state)
				(let* ((wr (connection-world cn)) (me (world-me wr)) (do
						(lambda () (check-and-treat cn hp-steady hp-danger mp-steady mp-reserve economy resurrect? distance))))
					(case-event event
						('change-target (subject-id target-id . rest)
							(and (= subject-id (object-id me)) target-id (in-party? (world-party wr) target-id) (do))
						)
						('creature-update (creature-id changes)
							(let ((hpc (ref changes 'hp)) (mpc (ref changes 'mp)))
								(and (in-party? (world-party wr) creature-id) (or
									(and hpc (< (car hpc) (cdr hpc)))
									(and mpc (< (car mpc) (cdr mpc)))
									(ref changes 'alike-dead?)
								) (do))
							)
						)
						('skill-launched (subject-id . rest)
							(when (= subject-id (object-id me)) (do))
						)
						('skill-canceled (subject-id . rest)
							(when (= subject-id (object-id me)) (do))
						)
						('skill-reusing (skill) (do))
						('skill-reused (skill) (do))
						('die (subject-id . rest) ; TODO Auto-logout if things going really bad.
							(when (and (in-party? (world-party wr) subject-id) resurrect?) (do))
						)
						('party-memeber-join rest (do))
						('party-memeber-leave rest
							(when (not (world-party wr)) (error-no-party))
						)
						('party-leave ()
							(when (not (world-party wr)) (error-no-party))
						)
					)

					(void)
				)
			)

			#:constructor (lambda (cn)
				(let* ((wr (connection-world cn)) (me (world-me wr)))
					(when (not (world-party wr)) (error-no-party))
					(when (ref me 'dead?) (program-error "Must be alive."))
					(when (and hp-steady (not (has-skills wr
							'heal 'greater-heal 'major-heal
							'group-heal 'greater-group-heal 'major-group-heal
							'vitalize 'restore-life 'benediction)))
						(program-error "No heal skill.")
					)
					(when (and hp-danger (not (has-skills wr 'battle-heal 'greater-battle-heal)))
						(program-error "No urgent heal skill.")
					)
					(when (and mp-steady (not (has-skills wr 'recharge)))
						(program-error "No recharge skill.")
					)
					(when (and resurrect? (not (has-skills wr 'resurrection 'mass-resurrection)))
						(program-error "No resurrection skill.")
					)

					(check-and-treat cn hp-steady hp-danger mp-steady mp-reserve economy resurrect? distance)
					(void)
				)
			)
		)
	)
)
