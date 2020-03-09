(module logic racket/base
	(require
		srfi/1
		(only-in racket/function negate)
		(rename-in racket/contract (any all/c))
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
		(cons 'position (negate equal?))
		(cons 'stackable? (negate eq?))
		(cons 'count (negate =))
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
		(struct-update (update-object object data) data item)
	)
	(define (update-item! object data)
		(let-values (((updated changes) (update-item (unbox object) data)))
			(set-box! object updated)
			changes
		)
	)
)
