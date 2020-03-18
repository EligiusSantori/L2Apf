(module logic racket/base
	(require
		srfi/1
		(only-in racket/function negate)
		(rename-in racket/contract (any all/c))
		"../library/geometry.scm"
		"../system/structure.scm"
		"object.scm"
	)

	(provide (contract-out
		(item? (-> any/c boolean?))
		(on-ground? (-> item? boolean?))
		(in-inventory? (-> item? boolean?))
		(make-item (-> list? box?))
		(update-item! (-> box? list? list?))
	))

	(define item (list
		(cons 'item-id (negate =))
		(cons 'position (negate point/3d=))
		(cons 'equipped? (negate eq?))
		(cons 'count (negate =))
		(cons 'enchant (negate =))

		; (cons 'stackable? (negate eq?))
		; TODO slot (symbol)
		; TODO weapon? shield? armor? jewelry? thing? quest?
	))

	(define (item? object)
		(if (and object (member 'item (ref object 'type))) #t #f)
	)

	(define (on-ground? item)
		(if (ref item 'position) #t #f)
	)

	(define (in-inventory? item)
		(not (on-ground? item))
	)

	(define (make-item data)
		(let ((object (make-object data)))
			(let ((type (cons 'item (ref object 'type))))
				(box (fold
					(lambda (p r) (if (and p (assoc (car p) item eq?)) (cons p r) r)) ; If field belongs to item.
					(cons (cons 'type type) (alist-delete 'type object))
					data
				))
			)
		)
	)

	(define (update-item object data)
		(struct-update data item (update-object object data))
	)
	(define (update-item! object data)
		(let-values (((rest updated changes) (update-item (unbox object) data)))
			(set-box! object (append rest updated))
			changes
		)
	)
)
