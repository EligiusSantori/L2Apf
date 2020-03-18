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
)
