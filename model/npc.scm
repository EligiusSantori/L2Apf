(module logic racket/base
	(require
		(only-in srfi/1 fold alist-delete)
		(only-in racket/function negate)
		racket/contract
		"../library/extension.scm"
		"../system/structure.scm"
		"object.scm"
		"creature.scm"
	)
	(provide (contract-out
		(npc? (-> any/c boolean?))
		(make-npc (-> list? box?))
		(update-npc! (-> box? list? list?))
		(boss? (-> npc? boolean?))
		(minion? (-> npc? boolean?))
	))

	(define npc (list
		(cons 'npc-id (negate =))
		(cons 'show-name? (negate eq?))
		(cons 'attackable? (negate eq?))
		(cons 'spoiled? (negate eq?))
		(cons 'summoned? (negate eq?))
	))

	(define (npc? object)
		(object-of-type? object 'npc)
	)

	(define (make-npc data)
		(let ((creature (make-creature data)))
			(box (fold
				(lambda (p r)
					(if (and p (assoc (car p) npc eq?)) ; If field belongs to npc.
						(if (eq? (car p) 'npc-id)
							(cons (cons 'name (alist-ref npcs (cdr p) "" =)) (cons p r))
							(cons p r)
						)
						r
					)
				)
				(fold (lambda (p r)
					(case (car p)
						((type) (cons (cons 'type (cons 'npc (cdr p))) r))
						((name) r) ; Drop name.
						(else (cons p r))
					)
				) (list) creature)
				data
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

	; TODO Use shared database.
	(define npcs (list
		(cons 10372 "Discarded Guardian")

		(cons 10378 "Madness Beast")
		(cons 10379 "Dementia Beast")

		(cons 10373 "Malex Herald of Dagoniel")
		(cons 10374 "Abyss Flyer")

		(cons 10380 "Kaysha Herald Of Ikaros")
		(cons 10381 "Nightmare Flyer")
		(cons 10382 "Hostile Flyer")

		(cons 10001 "Greyclaw Kutus")
		(cons 10002 "Guard of Kutus")
		(cons 10003 "Pawn of Kutus")
	))

	(define bosses (list 10372 10373 10378 10380 10001))
	(define minions (list
		10374
		10379
		10381 10382
		10002 10003
	))
	(define (boss? npc) (if (member (ref npc 'npc-id) bosses =) #t #f))
	(define (minion? npc) (if (member (ref npc 'npc-id) minions =) #t #f))
)
