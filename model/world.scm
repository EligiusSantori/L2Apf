(module logic racket/base
	(require
		srfi/1
		racket/set
		(rename-in racket/contract (any all/c))
		"../library/ral.scm"
		"../library/extension.scm"
		"../library/structure.scm"
		"skill.scm"
		"object.scm"
		"creature.scm"
		"npc.scm"
		"character.scm"
	)
	(provide (contract-out
		(world? (any/c . -> . boolean?))
		(make-world ((listof pair?) . -> . world?))
		(register-object! (world? object? . -> . void?))
		(discard-object! (world? integer? . -> . void?))
		(objects (->* (world?) (procedure?) list?))
		(object-ref (world? (or/c integer? false/c) . -> . (or/c box? false/c)))
		(skill-ref (world? (or/c integer? false/c) . -> . (or/c skill? false/c)))
		(find-character-by-name (world? string? . -> . (or/c character? false/c)))
		(get-target (world? creature? . -> . (or/c creature? false/c)))
		(get-level (creature? . -> .  (or/c integer? false/c)))
		(attackable? (any/c . -> . boolean?))
	))
	
	(define (world? a)
		(hash? a) ; TODO
	)
	
	(define (make-world server)
		(let ((world (make-hash server)))
			(hash-set! world 'me #f)
			(hash-set! world 'skills (make-hash))
			(hash-set! world 'quests (make-hash))
			(hash-set! world 'party (list))
			(hash-set! world 'clans (make-hash))
			(hash-set! world 'alliances (make-hash))
			;(hash-set! world 'inventory (mutable-set)) ; TODO Как я должен оперировать из кода с таким объектом? Это нарушает абстракцию.
			world
		)
	)

	(define (register-object! world object)
		(let ((object-id (@: object 'object-id)))
			(hash-set! world object-id object)
			; TODO set 'characters name object-id
			; TODO set ski
			(void)
		)
	)
	(define (discard-object! world object-id)
		(if (hash-has-key? world object-id)
			(hash-remove! world object-id)
			(void)
		)
	)

	(define (objects world [predicate always?])
		(map cdr (hash-filter world (lambda (k v)
			(and (integer? k) (predicate v))
		)))
	)
	
	(define (object-ref world object-id)
		(hash-ref world object-id #f)
	)
	
	(define (skill-ref world skill-id)
		(hash-ref (hash-ref world 'skills) skill-id #f)
	)
	
	;(define (inventory-ref world object-id)
	
	;)
	
	(define (find-character-by-name world name)
		(define (test? k v)
			(and (integer? k) (character? v) (string-ci=? (@: v 'name) name))
		)
	
		(let ((found (hash-find world test?)))
			(if found (cdr found) #f)
		)
	)
	
	(define (get-target world creature)
		(object-ref world (@: creature 'target-id))
	)
	
	(define (attackable? creature)
		(and
			creature
			(creature? creature)
			(alive? creature)
			(or
				(character? creature)
				(and
					(npc? creature)
					(@: creature 'attackable?)
				)
			)
		)
	)
	
	(define (get-level creature)
		(or
			(and (character? creature)
				(@: creature 'level)
			)
			(and (npc? creature)
				(let ((match (regexp-match (pregexp "(?i:Lv)\\s*(\\d+)") (or (@: creature 'title) ""))))
					(and match (string->number (last match)))
				)
			)
		)
	)
)
