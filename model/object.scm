(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
		srfi/1
		"../library/extension.scm"
		"../system/structure.scm"
	)
	(provide (contract-out
		(object? (any/c . -> . boolean?))
		(object-id (object? . -> . (or/c integer? #f)))
		(object=? (object? object? . -> . boolean?))
		(create-object (list? . -> . list?))
		(update-object (list? list? . -> . list?))
	))

	(define (object? object)
		(if (and (box? object) (member 'object (@: object 'type))) #t #f)
	)

	(define (object-id object)
		(ref object 'object-id)
	)

	(define (object=? a b)
		(= (object-id a) (object-id b))
	)

	(define (create-object struct)
		(list
			(cons 'type (list 'object))
			(cons 'object-id (@: struct 'object-id))
		)
	)

	(define (update-object object struct)
		(let ((object-id (alist-ref struct 'object-id #f eq?)))
			; (let ((id (alist-ref object 'object-id #f eq?)))
			; 	(when (and id object-id (or (not (integer? object-id)) (not (= id object-id))))
			; 		(error "Invalid id passed to object." id object-id)
			; 	)
			; )

			(if object-id
				(cons (cons 'object-id object-id) (remove (lambda (p)
					(eq? (car p) 'object-id)
				) object))
				object
			)
		)
	)
)
