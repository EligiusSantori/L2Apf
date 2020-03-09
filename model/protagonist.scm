(module logic racket/base
	(require
		srfi/1
		(only-in racket/function negate)
		(rename-in racket/contract (any all/c))
		"../library/extension.scm"
		"../system/structure.scm"
		"character.scm"
	)
	(provide (contract-out
		(protagonist? (-> any/c boolean?))
		(make-protagonist (-> list? box?))
		(update-protagonist! (-> box? list? list?))
	))

	(define protagonist (list
		(cons 'sp (negate =))
		(cons 'xp (negate =))
		(cons 'load (negate =))
		(cons 'pk-count (negate =))
		(cons 'pvp-count (negate =))

		(cons 'died? (negate eq?))
		(cons 'clan-leader? (negate eq?))
		(cons 'dwarven-craft? (negate eq?))

		(cons 'character-id (negate =))
		(cons 'base-class-id (negate =))
		(cons 'max-load (negate =))
		(cons 'inventory-limit (negate =))
		(cons 'access-level (negate =))
		(cons 'physical-attack-power (negate =))
		(cons 'physical-defense (negate =))
		(cons 'magical-attack-power (negate =))
		(cons 'magical-defense (negate =))
		(cons 'accuracy (negate =))
		(cons 'evasion (negate =))
		(cons 'focus (negate =))

		(cons 'statements (negate alist-equal?))
		(cons 'equipment (negate alist-equal?))
	))

	(define (protagonist? object)
		(if (and object (member 'protagonist (ref object 'type))) #t #f)
	)

	(define (make-protagonist data)
		(let ((character (make-character data)))
			(let ((type (cons 'protagonist (ref character 'type))))
				(box (fold
					(lambda (p r) (if (and p (assoc (car p) protagonist eq?)) (cons p r) r)) ; If field belongs to protagonist.
					(cons (cons 'type type) (alist-delete 'type character))
					data
				))
			)
		)
	)

	(define (update-protagonist object data)
		(let-values (((character cp) (update-character object data)))
			(let-values (((updated cc) (struct-update character data protagonist)))
				(values updated (append cp cc))
			)
		)
	)
	(define (update-protagonist! object data)
		(let-values (((updated changes) (update-protagonist (unbox object) data)))
			(set-box! object updated)
			changes
		)
	)
)
