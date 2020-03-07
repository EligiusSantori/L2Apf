(module logic racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
		"../system/structure.scm"
	)
	(provide (contract-out
		(skill? (any/c . -> . boolean?))
		(skill-id (skill? . -> . integer?))
		(skill-ready? ((or/c any/c false/c) . -> . boolean?))
		(make-skill (->* (integer? integer?) (boolean? integer? integer?) skill?))
	))

	(define (skill? skill)
		(if (and skill (@: skill 'skill-id)) #t #f)
	)

	(define (skill-id skill)
		(ref skill 'skill-id)
	)

	(define (skill-ready? skill)
		(if (skill? skill)
			(let ((last-usage (@: skill 'last-usage)) (reuse-delay (@: skill 'reuse-delay)))
				(and (@: skill 'active?) (> (current-milliseconds) (+ last-usage reuse-delay)))
			)
			#f
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
