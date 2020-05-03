(module logic racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
		"../library/date_time.scm"
		"../system/structure.scm"
	)
	(provide (contract-out
		(skill? (-> any/c boolean?))
		(skill-id (-> skill? integer?))
		(skill-ready? (-> skill? boolean?))
		(make-skill (->* (integer? integer?) (boolean? rational? rational?) skill?))

		(skill-positive? (-> integer? boolean?))
		(skill-negative? (-> integer? boolean?))
	))

	(define (skill? skill)
		(if (and skill (ref skill 'skill-id)) #t #f)
	)

	(define (skill-id skill)
		(ref skill 'skill-id)
	)

	(define (skill-ready? skill)
		(let ((last-usage (ref skill 'last-usage)) (reuse-delay (ref skill 'reuse-delay)))
			(and (ref skill 'active?) (> (timestamp) (+ last-usage reuse-delay)))
		)
	)

	(define (make-skill skill-id level [active? #t] [last-usage 0] [reuse-delay 0])
		(list
			(cons 'skill-id skill-id)
			(cons 'level level)
			(cons 'active? active?)
			(cons 'last-usage last-usage)
			(cons 'reuse-delay reuse-delay)
		)
	)

	(define (skill-positive? skill-id)
		(if (assoc skill-id (list
			(cons 1011 'heal)
			(cons 1015 'battle-heal)
			(cons 1027 'group-heal)
			(cons 1217 'greater-heal)
			(cons 1218 'greater-battle-heal)
			(cons 1219 'greater-group-help)

			(cons 1013 'recharge)

			(cons 1012 'cure-poision)
			(cons 1018 'purify)
			(cons 1020 'vitalize)

			(cons 1016 'resurrection)
			(cons 1254 'mass-resurrection)
			; (cons 1255 'party-recall)

			(cons 1035 'mental-shield)
			(cons 1040 'shield)
			(cons 1059 'empower)
			(cons 1068 'might)
			(cons 1077 'focus)
			(cons 1078 'concentration)
			(cons 1204 'wind-walk)
			(cons 1240 'guidance)
			(cons 1242 'death-whisper)
			(cons 1268 'vampiric-rage)
		) =)  #t #f)
	)
	(define (skill-negative? skill-id)
		(not (skill-positive? skill-id))
	)
)
