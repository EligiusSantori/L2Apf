(module logic racket/base
	(require
		(only-in srfi/1 fold proper-list?)
		(only-in racket/function negate)
		racket/contract
		"../library/date_time.scm"
		"../library/extension.scm"
		"../system/structure.scm"
		"../system/database.scm"
	)
	(provide (contract-out
		(skill? (-> any/c boolean?))
		(skill-id (-> skill? integer?))
		(skill-ready? (-> skill? boolean?))
		(make-skill (-> list? any/c box?))
		(update-skill! (-> box? list? list?))
		(set-skill-used! (->* (skill?) (rational? rational?) any))
		(skills (->* (hash?) (procedure?) list?))
		(fold-skills (-> hash? any/c procedure? any))
	))

	(define skill (list
		(cons 'id (negate =))
		(cons 'level (negate =))
		(cons 'active? (negate eq?))
		(cons 'enabled? (negate eq?))
		(cons 'last-usage (negate =))
		(cons 'reuse-delay (negate =))
		(cons 'type #f)
	))

	(define (skill? skill)
		(if (and skill (box? skill) (proper-list? (unbox skill)) (member 'skill (ref skill 'type) eq?)) #t #f)
	)
	(define (skill-id skill)
		(ref skill 'id)
	)
	(define (skill-ready? skill)
		(let ((last-usage (ref skill 'last-usage)) (reuse-delay (ref skill 'reuse-delay)))
			(and (ref skill 'active?) (> (timestamp) (+ last-usage reuse-delay)))
		)
	)

	(define (make-skill data db) ; TODO Can be optimized.
		(let ((id (ref data 'skill-id)))
			(box (append
				(db-skill db id)
				(list
					(cons 'id id)
					(cons 'type (list 'skill))
					(assq 'level data)
					(assq 'active? data)
					(cons 'last-usage (or (ref data 'last-usage) 0))
					(cons 'reuse-delay (or (ref data 'reuse-delay) 0))
				)
			))
		)
	)
	(define (update-skill s data)
		(struct-update data skill s)
	)
	(define (update-skill! skill data)
		(let-values (((rest updated changes) (update-skill (unbox skill) data)))
			(set-box! skill (append rest updated))
			changes
		)
	)
	(define (set-skill-used! skill [last-usage (timestamp)] [reuse-delay #f])
		(update-skill! skill (list
			(cons 'last-usage last-usage)
			(and reuse-delay (cons 'reuse-delay reuse-delay))
		))
		(void)
	)

	(define (skills sk [predicate #f])
		(if predicate
			(hash-filter sk predicate #f)
			(hash-values sk)
		)
	)
	(define (fold-skills sk init proc)
		(define r init)
		(hash-for-each sk (lambda (k v)
			(set! r (proc v r))
		))
		r
	)
)
