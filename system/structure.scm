(module system racket/base
	(require
		(only-in srfi/1 fold car+cdr)
		(only-in racket/dict dict-ref)
		(rename-in racket/contract (any all/c))
		"../library/extension.scm"
	)
	(provide (contract-out
		(ref (->* ((or/c list? box?)) #:rest list? any/c))
		(struct-update (->* (list? list? list?) (list? list?) (values list? list? list?)))
	))

	(define (ref struct . chain) ; Ultimate dictionary path extractor.
		(define (f a b) (if (and b a) (dict-ref (if (box? b) (unbox b) b) a #f) #f))
		(fold f (f (car chain) struct) (cdr chain))
	)

	(define (struct-extract lst key [rst (list)]) ; O(n)
		(if (not (null? lst))
			(let ((p (car lst)))
				(if p
					(if (eq? key (car p))
						(values (cdr p) (append rst (cdr lst))) ; Pair found.
						(struct-extract (cdr lst) key (cons p rst)) ; Iterate next.
					)
					(struct-extract (cdr lst) key rst) ; Empty pair.
				)
			)
			(values #f rst) ; Pair not found.
		)
	)

	(define (field-update struct field new allow?)
		(let-values (((old rest) (struct-extract struct field))) ; O(n)
			(if (and allow? (or (eq? #t allow?) (and (not old) new) (and (not new) old) (and old new (allow? old new))))
				(cons rest (cons new old))
				#f ; Change forbidden or value hasn't changed.
			)
		)
	)

	(define (struct-update data fields rest [updated (list)] [changes (list)])
		(if (not (null? data))
			(if (car data)
				(let-values (((field value) (car+cdr (car data))))
					(let ((allow? (alist-ref fields field #f eq?))) ; O(n/2)
						(let ((update (field-update rest field value allow?)))
							(if update
								(struct-update (cdr data) fields (car update)
									(cons (cons field (cadr update)) updated)
									(if (not (eq? #t allow?))
										(cons (cons field (cdr update)) changes) ; Add change record to chain.
										changes ; Skip change record for service field.
									)
								)
								(struct-update (cdr data) fields rest updated changes) ; Field not changed.
							)
						)
					)
				)
				(struct-update (cdr data) fields rest updated changes) ; Invalid data pair.
			)
			(values rest updated changes) ; End of data.
		)
	)
)
