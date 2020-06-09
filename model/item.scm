(module logic racket/base
	(require
		(only-in srfi/1 fold alist-delete)
		(only-in racket/function negate)
		racket/contract
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
		(cons 'position (negate point/3d=?))
		(cons 'count (negate =))
		; (cons 'quest? (negate eq?))
		; (cons 'type (negate eq?)) ; ... 'scroll 'potion 'blunt
		(cons 'enchant (negate =))
		; TODO stackable? equipped? grade crystallizable? {weapon? shield? armor? accessory? thing? quest?}
	))

	; ? (define weapon? ... type in 'dagger 'sword ...)
	; (define armor? ... type in 'body 'legs ...)
	; (define jewelry? ... type in 'earing 'ring ...)
	; ? (define usable? ...)
	; ? (define equipable? ...)

	(define (item? object)
		(object-of-type? object 'item)
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
