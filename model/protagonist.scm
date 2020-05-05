(module logic racket/base
	(require
		srfi/1
		(only-in racket/set set->list)
		(only-in racket/function negate)
		(rename-in racket/contract (any all/c))
		"../library/extension.scm"
		"../system/structure.scm"
		"../system/cache.scm"
		"character.scm"
	)
	(provide (contract-out
		(protagonist? (-> any/c boolean?))
		(make-protagonist (-> list? box?))
		(update-protagonist! (-> box? list? list?))
		(attackers (-> box? list?))
		(attackers-add! (-> box? integer? void?))
		(attackers-delete! (-> box? integer? void?))
		(attackers-clear! (-> box? void?))
		(attackers-has? (-> box? integer? boolean?))
		(attackers-count (-> box? integer?))
	))

	(define protagonist (list
		(cons 'sp (negate =))
		(cons 'xp (negate =))
		(cons 'load (negate =))
		(cons 'pk-count (negate =))
		(cons 'pvp-count (negate =))

		(cons 'dead? (negate eq?))
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
		(cons 'attackers #f)
	))

	(define (protagonist? object)
		(if (and object (member 'protagonist (ref object 'type))) #t #f)
	)

	(define (make-protagonist data)
		(let ((character (make-character data)))
			(let ((type (cons 'protagonist (ref character 'type))))
				(box (fold
					(lambda (p r) (if (and p (assoc (car p) protagonist eq?)) (cons p r) r)) ; If field belongs to protagonist.
					(append (alist-delete 'type character) (list
						(cons 'type type)
						(cons 'attackers (make-cache-set 60 =)) ; Default aggro timeout.
					))
					data
				))
			)
		)
	)

	(define (update-protagonist object data)
		(let-values (((rest updated changes) (update-character object data)))
			(struct-update data protagonist rest updated changes)
		)
	)
	(define (update-protagonist! object data)
		(let-values (((rest updated changes) (update-protagonist (unbox object) data)))
			(set-box! object (append rest updated))
			changes
		)
	)

	(define (attackers protagonist)
		(set->list (cache-set-all (ref protagonist 'attackers)))
	)
	(define (attackers-add! protagonist object-id)
		(cache-set-add! (ref protagonist 'attackers) object-id)
	)
	(define (attackers-delete! protagonist object-id)
		(cache-set-delete! (ref protagonist 'attackers) object-id)
	)
	(define (attackers-clear! protagonist)
		(cache-set-clear! (ref protagonist 'attackers))
	)
	(define (attackers-has? protagonist object-id)
		(cache-set-has? (ref protagonist 'attackers) object-id)
	)
	(define (attackers-count protagonist)
		(cache-set-count (ref protagonist 'attackers))
	)
)
