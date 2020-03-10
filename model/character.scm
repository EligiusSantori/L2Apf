(module logic racket/base
	(require
		srfi/1
		(only-in racket/function negate)
		(rename-in racket/contract (any all/c))
		"../system/structure.scm"
		"creature.scm"
	)
	(provide (contract-out
		(character? (-> any/c boolean?))
		(make-character (-> list? list?))
		(update-character (-> list? list? (values list? list? list?)))
		(update-character! (-> box? list? list?))
	))

	(define character (list
		(cons 'level (negate =))
		(cons 'cp (negate =))
		(cons 'max-cp (negate =))
		(cons 'karma (negate =))

		(cons 'race (negate =))
		(cons 'gender (negate eq?))
		(cons 'face-type (negate =))
		(cons 'hair-style (negate =))
		(cons 'hair-color (negate =))
		(cons 'name-color (negate =))
		(cons 'class-id (negate =))

		(cons 'pvp? (negate eq?))
		(cons 'invisible? (negate eq?))
		(cons 'find-party? (negate eq?))
		(cons 'hero-icon? (negate eq?))
		(cons 'hero-glow? (negate eq?))
		(cons 'fishing? (negate eq?))
		(cons 'mount-type (negate eq?))
		(cons 'private-store (negate eq?))

		(cons 'clan-id (negate =))
		(cons 'clan-crest-id (negate =))
		(cons 'ally-id (negate =))
		(cons 'ally-crest-id (negate =))

		(cons 'cubics (negate =))
		(cons 'fish (negate =))
	))

	(define (character? object)
		(if (and object (member 'character (ref object 'type))) #t #f)
	)

	(define (make-character data)
		(let ((creature (make-creature data)))
			(let ((type (cons 'character (ref creature 'type))))
				(fold
					(lambda (p r) (if (and p (assoc (car p) character eq?)) (cons p r) r)) ; If field belongs to character.
					(cons (cons 'type type) (alist-delete 'type creature))
					data
				)
			)
		)
	)

	(define (update-character object data)
		(let-values (((rest updated changes) (update-creature object data)))
			(struct-update data character rest updated changes)
		)
	)
	(define (update-character! object data)
		(let-values (((rest updated changes) (update-character (unbox object) data)))
			(set-box! object (append rest updated))
			changes
		)
	)
)
