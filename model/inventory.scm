(module logic racket/base
	(require
		racket/contract
		"../system/structure.scm"
	)
	(provide (contract-out
		(items (->* (hash?) (procedure?) list?))
		(fold-items (-> hash? any/c procedure? any))
		(find-items (->* (hash?) #:rest list? list?))
	))

	(define (items inv [predicate #f])
		(define l (list))
		(if predicate
			(begin
				(hash-for-each inv (lambda (k v)
					(when (predicate v) (set! l (cons v l)))
				))
				l
			)
			(hash-values inv)
		)
	)

	(define (fold-items inv init proc)
		(define r init)
		(hash-for-each inv (lambda (k v)
			(set! r (proc v r))
		))
		r
	)

	(define (find-items inv . item-ids)
		(fold-items inv (list) (lambda (item r)
			(if (member (ref item 'item-id) item-ids =)
				(cons item r)
				r
			)
		))
	)
)
