(module system racket/base
	(require
		srfi/1
		(only-in racket/dict dict-ref)
		(rename-in racket/contract (any all/c))
		"../library/extension.scm"
	)
	(provide (contract-out
		(ref (->* ((or/c list? box?)) #:rest list? any/c))
		(struct-update (-> list? list? list? (values list? list?)))
	))

	(define (ref struct . chain) ; Ultimate dictionary path extractor.
		(define (f a b) (if (and b a) (dict-ref (if (box? b) (unbox b) b) a #f) #f))
		(fold f (f (car chain) struct) (cdr chain))
	)

	(define (struct-extract lst key [rst (list)])
		(if (not (null? lst))
			(let ((p (car lst)))
				(if p
					(if (eq? key (car p))
						(values (cdr p) (append (cdr lst) rst)) ; Pair found.
						(struct-extract (cdr lst) key (cons p rst)) ; Iterate next.
					)
					(struct-extract (cdr lst) key rst) ; Empty pair.
				)
			)
			(values #f rst) ; Pair not found.
		)
	)

	(define (field-update struct field new allow?)
		(let-values (((old rest) (struct-extract struct field)))
			(if (and allow? (or (eq? #t allow?) (and (not old) new) (and (not new) old) (allow? old new)))
				(cons (cons (cons field new) rest) old) ; Return struct & old value.
				#f ; Change forbidden or value hasn't changed.
			)
		)
	)

	(define (struct-update struct data fields)
		(car+cdr (fold (lambda (pair r) (let-values (((field value) (car+cdr pair)))
			(let ((allow? (alist-ref fields field #f eq?)))
				(let ((update (field-update (car r) field value allow?)))
					(if update
						(if (eq? #t allow?)
							(cons (car update) (cdr r)) ; Skip change record for service field.
							(cons (car update) (cons (cons field (cons (cdr update) value)) (cdr r))) ; Return struct & add change to chain.
						)
						r ; Field not changed.
					)
				)
			)
		)) (cons struct (list)) data))
	)
)
