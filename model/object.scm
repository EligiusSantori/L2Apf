(module logic racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
		"../library/extension.scm"
		"../system/structure.scm"
	)
	(provide (contract-out
		(object? (any/c . -> . boolean?))
		(object-id (object? . -> . (or/c integer? #f)))
		(object=? (object? object? . -> . boolean?))
		(make-object (list? . -> . list?))
		(update-object (list? list? . -> . list?))
	))

	(define (object? object)
		(if (and (box? object) (member 'object (ref object 'type))) #t #f)
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
