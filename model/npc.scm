(module logic racket/base
	(require
		(only-in srfi/1 fold)
		(only-in racket/function negate)
		racket/contract
		"../library/extension.scm"
		"../system/structure.scm"
		"../system/database.scm"
		"object.scm"
		"creature.scm"
	)
	(provide (contract-out
		(npc? (-> any/c boolean?))
		(make-npc (-> list? any/c box?))
		(update-npc! (-> box? list? list?))

		(person? (-> npc? boolean?))
		(monster? (-> npc? boolean?))
		(minion? (-> npc? boolean?))
		(boss? (-> npc? boolean?))
	))

	(define npc (list
		(cons 'npc-id (negate =))
		(cons 'show-name? (negate eq?))
		(cons 'attackable? (negate eq?))
		(cons 'spoiled? (negate eq?))
		(cons 'summoned? (negate eq?))

		(cons 'npc-type #f)
		(cons 'aggro #f)
		(cons 'group #f)
	))

	(define (npc? object)
		(object-of-type? object 'npc)
	)

	(define (make-npc data db) ; TODO Can be optimized.
		(let* ((creature (make-creature data)) (type (cons 'npc (ref creature 'type))))
			(box (append
				(list (cons 'type type))
				(db-npc db (ref data 'npc-id))
				(fold ; TODO extract npc-id
					(lambda (p r) (if (and p (assoc (car p) npc eq?)) (cons p r) r)) ; If field belongs to npc.
					(alist-except creature eq? 'type 'name 'level) ; TODO extract type
					data
				)
			))
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

	(define (person? npc)
		(eq? (ref npc 'npc-type) 'person)
	)
	(define (monster? npc)
		(eq? (ref npc 'npc-type) 'monster)
	)
	(define (minion? npc)
		(eq? (ref npc 'npc-type) 'minion)
	)
	(define (boss? npc)
		(eq? (ref npc 'npc-type) 'boss)
	)
)
