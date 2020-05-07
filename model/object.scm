(module logic racket/base
	(require
		(only-in srfi/1 proper-list?)
		racket/contract
		"../library/extension.scm"
		"../system/structure.scm"
	)
	(provide (contract-out
		(object? (-> any/c boolean?))
		(object-of-type? (-> any/c symbol? boolean?))
		(object-id (-> object? (or/c integer? #f)))
		(object=? (-> object? object? boolean?))
		(make-object (-> list? list?))
		(update-object (-> list? list? list?))
	))

	(define (object-of-type? object type)
		(if (and object (box? object)
			(let ((data (unbox object)))
				(and
					(proper-list? data)
					(member type (or (ref data 'type) (list)))
				)
			)
		) #t #f)
	)
	(define (object? object)
		(object-of-type? object 'object)
	)

	(define (object-id object)
		(ref object 'object-id)
	)

	(define (object=? a b)
		(= (object-id a) (object-id b))
	)

	(define (make-object struct)
		(let ((object-id (ref struct 'object-id)))
			(if (integer? object-id)
				(list
					(cons 'type (list 'object))
					(cons 'object-id object-id)
				)
				(list (cons 'type (list 'object))) ; (error "Invalid id passed to object." object-id)
			)
		)
	)

	(define (update-object object data)
		(let ((nid (assoc 'object-id data eq?)))
			(if nid
				(let ((object-id (cdr nid)) (oid (assoc 'object-id object eq?)))
					(cond
						((and (not oid) (integer? object-id)) ; Set object-id.
							(cons (cons 'object-id object-id) object)
						)
						((or (not (integer? object-id)) (not (= (cdr oid) object-id))) ; Invalid object-id.
							(error "Invalid id passed to object." (cdr oid) object-id)
						)
						(else object) ; Valid object-id.
					)
				)
				object ; Nothing to update.
			)
		)
	)
)
