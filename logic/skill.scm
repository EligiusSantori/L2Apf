(module logic racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
		"../library/structure.scm"
	)
	(provide (contract-out
		(skill? (any/c . -> . boolean?))
		(skill-ready? ((or/c any/c false/c) . -> . boolean?))
		(make-skill (integer? integer? boolean? . -> . skill?))
	))
	
	(define (skill? skill)
		(if (@: skill 'skill-id) #t #f)
	)
	
	(define (skill-ready? skill)
		(if (skill? skill)
			(let ((last-usage (@: skill 'last-usage)) (reuse-delay (@: skill 'reuse-delay)))
				(and (@: skill 'active?) (> (current-milliseconds) (+ last-usage reuse-delay)))
			)
			#f
		)
	)
	
	(define (make-skill skill-id level active?)
		(list
			(cons 'skill-id skill-id)
			(cons 'level level)
			(cons 'active? active?)
			(cons 'last-usage 0)
			(cons 'reuse-delay 0)
		)
	)
)