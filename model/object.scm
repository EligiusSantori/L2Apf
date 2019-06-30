(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
		srfi/1
		"../library/extension.scm"
		"../library/structure.scm"
	)
	(provide (contract-out
		(object? (any/c . -> . boolean?))
		(object=? (object? object? . -> . boolean?))
		(create-object (list? . -> . list?))
		(update-object (list? list? . -> . list?))
	))

	(define (object? object)
		(if (and (box? object) (member 'object (@: object 'type))) #t #f)
	)

	(define (object=? a b)
		(= (@: a 'object-id) (@: b 'object-id))
	)

	(define (create-object struct)
		(list
			(cons 'type (list 'object))
			(cons 'object-id (@: struct 'object-id))
		)
	)

	(define (update-object object struct)
		(let ((object-id (alist-ref struct 'object-id)))
			(if object-id (alist-cons 'object-id object-id object) object)
		)
	)
)
