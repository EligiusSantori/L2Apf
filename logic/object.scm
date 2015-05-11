(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
		srfi/1
		"../library/ral.scm"
		"../library/structure.scm"
		"main.scm"
	)
	(provide (contract-out
		(object? (any/c . -> . boolean?))
		(object=? (object? object? . -> . boolean?))
		(create-object (list? . -> . list?))
		(update-object (list? list? . -> . list?))
		(objects-angle (box? box? . -> . (or/c real? false/c)))
		(objects-distance (box? box? . -> . integer?))
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
		(let ((object-id (alist-ref 'object-id struct)))
			(if object-id (alist-cons 'object-id object-id object) object)
		)
	)
	
	(define (objects-angle a b)
		(points-angle (@: a 'position) (@: b 'position))
	)
	
	(define (objects-distance a b)
		(points-distance (@: a 'position) (@: b 'position))
	)
)