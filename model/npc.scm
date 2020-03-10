(module logic racket/base
	(require
		srfi/1
		(only-in racket/function negate)
		(rename-in racket/contract (any all/c))
		"../system/structure.scm"
		"creature.scm"
	)
	(provide (contract-out
		(npc? (-> any/c boolean?))
		(make-npc (-> list? box?))
		(update-npc! (-> box? list? list?))
	))

	(define npc (list
		(cons 'npc-id (negate =))
		(cons 'show-name? (negate eq?))
		(cons 'attackable? (negate eq?))
		(cons 'spoiled? (negate eq?))
		(cons 'summoned? (negate eq?))
	))

	(define (npc? object)
		(if (and object (member 'npc (ref object 'type))) #t #f)
	)

	(define (make-npc data)
		(let ((creature (make-creature data)))
			(let ((type (cons 'npc (ref creature 'type))))
				(box (fold
					(lambda (p r) (if (and p (assoc (car p) npc eq?)) (cons p r) r)) ; If field belongs to npc.
					(cons (cons 'type type) (alist-delete 'type creature))
					data
				))
			)
		)
	)

	(define (update-npc object data)
		(let-values (((rest updated changes) (update-creature object data)))
			(struct-update data npc rest updated changes)
		)
	)
	(define (update-npc! object data)
		(let-values (((rest updated changes) (update-npc (unbox object) data)))
			(set-box! object (append rest updated))
			changes
		)
	)
)
