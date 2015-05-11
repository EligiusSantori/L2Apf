(module logic racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
		"../library/ral.scm"
		"../library/structure.scm"
		"object.scm"
		"character.scm"
	)
	(provide (contract-out
		(register-object! (hash? object? . -> . void?))
		(discard-object! (hash? (or/c object? integer?) . -> . void?))
		(get-object (hash? integer? . -> . box?))
		(find-character-by-name (box? string? . -> . (or/c box? false/c)))
	))

	(define (register-object! world object)
		(let ((object-id (@: object 'object-id)))
			(hash-set! world object-id object)
			(void)
		)
	)
	(define (discard-object! world object)
		(let ((object-id (if (integer? object) object (@: object 'object-id))))
			(if (hash-has-key? world object-id)
				(hash-remove! world object-id)
				(void)
			)
		)
	)

	(define (get-object world id)
		(hash-ref world id #f)
	)
	
	(define (find-character-by-name world name)
		(hash-find world (lambda (k v)
			(and (integer? k) (character? v) (equal? (@: v 'name) name))
		))
	)
)