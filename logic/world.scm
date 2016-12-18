(module logic racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
		"../library/ral.scm"
		"../library/structure.scm"
		"skill.scm"
		"object.scm"
		"character.scm"
	)
	(provide (contract-out
		(world? (any/c . -> . boolean?))
		(make-world ((listof pair?) . -> . world?))
		(register-object! (world? object? . -> . void?))
		(discard-object! (world? integer? . -> . void?))
		(object-ref (world? (or/c integer? false/c) . -> . (or/c box? false/c)))
		(skill-ref (world? (or/c integer? false/c) . -> . (or/c skill? false/c)))
		(find-character-by-name (box? string? . -> . (or/c box? false/c)))
	))
	
	(define (world? a)
		(hash? a) ; TODO temporary
	)
	
	(define (make-world server)
		(let ((world (make-hash server)))
			(hash-set! world 'me #f)
			(hash-set! world 'skills (make-hash))
			(hash-set! world 'quests (make-hash))
			(hash-set! world 'party (make-hash))
			(hash-set! world 'clans (make-hash))
			(hash-set! world 'alliances (make-hash))
			;(hash-set! world 'inventory (mutable-set))
			world
		)
	)

	(define (register-object! world object)
		(let ((object-id (@: object 'object-id)))
			(hash-set! world object-id object)
			; TODO set character name key
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

	(define (object-ref world object-id)
		(hash-ref world object-id #f)
	)
	
	(define (skill-ref world skill-id)
		(hash-ref (hash-ref world 'skills) skill-id #f)
	)
	
	;(define (inventory-ref world object-id)
	
	;)
	
	(define (find-character-by-name world name)
		(hash-find world (lambda (k v)
			(and (integer? k) (character? v) (equal? (@: v 'name) name))
		))
	)
)