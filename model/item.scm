(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
		srfi/1
		"../library/structure.scm"
		"object.scm"
	)

	(provide (contract-out
		(item? ((or/c box? false/c) . -> . boolean?))
		(on-ground? (item? . -> . boolean?))
		(in-inventory? (item? . -> . boolean?))
		(create-item (list? . -> . box?))
		(update-item! (box? list? . -> . void?))
	))

	(define (item? object)
		(if (and object (member 'item (@: object 'type))) #t #f)
	)

	(define (on-ground? item)
		(if (@: item 'position) #t #f)
	)

	(define (in-inventory? item)
		(not (on-ground? item))
	)

	(define (create-item struct)
		(let ((item (create-object struct)))
			(let ((type (cons 'item (@: item 'type))))
				(box (append (alist-delete 'type item) (list
					(cons 'type type)

					(cons 'item-id (@: struct 'item-id))
					(cons 'position (@: struct 'position))
					(cons 'stackable? (@: struct 'stackable?))
					(cons 'count (@: struct 'count))
				)))
			)
		)
	)

	(define (update-item! item struct)
		(set-box! item
			(let ((item (update-object (unbox item) struct)))
				(struct-transfer item struct
					'item-id
					'position
					'stackable?
					'count
				)
			)
		)
	)
)
