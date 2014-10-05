; Здесь все функции игровой логики, они не зависят от connection и оперируют чистыми данными
(module system racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
		"extension.scm"
		"ral.scm"
		"geometry.scm"
		"structure.scm"
	)
	(provide (contract-out
		(register-object! (hash? object? . -> . void?))
		(discard-object! (hash? object? . -> . void?))
		
		(find-character-by-name (box? string? . -> . (or/c box? false/c)))
		
		(create-protagonist (list? . -> . box?))
		(create-antagonist (list? . -> . box?))
		(create-npc (list? . -> . box?))
		(update-protagonist! (box? list? . -> . void?))
		(update-antagonist! (box? list? . -> . void?))
		(update-npc! (box? list? . -> . void?))
		
		(object? (box? . -> . boolean?))
		(creature? (box? . -> . boolean?))
		(npc? (box? . -> . boolean?))
		(character? (box? . -> . boolean?))
		(antagonist? (box? . -> . boolean?))
		(protagonist? (box? . -> . boolean?))
		
		(objects-angle (box? box? . -> . integer?))
		(objects-distance (box? box? . -> . integer?))
		
		(creature-angle (box? . -> . integer?))
		;( ( . -> . ))
	))
	
	(define (register-object! world object)
		(let ((object-id (@: object 'object-id)))
			(hash-set! world (@: object 'object-id) object)
			(void)
		)
	)
	(define (discard-object! world object)
		(let ((object-id (@: object 'object-id)))
			(if (hash-has-key? world object-id)
				(hash-remove! world object-id)
				(void)
			)
		)
	)
	
	(define (create-object struct)
		(list
			(cons 'type (list 'object))
			(cons 'object-id (@: struct 'object-id))
		)
	)
	(define (create-creature struct)
		(let ((object (create-object struct)))
			(let ((type (cons 'creature (@: object 'type))))
				(append (alist-delete 'type object) (list
					(cons 'type type)
					(cons 'hp (@: struct 'hp))
					(cons 'mp (@: struct 'mp))
					(cons 'position (@: struct 'position))
					(cons 'destination (@: struct 'destination))
				))
			)
		)
	)
	(define (create-npc struct)
		(let ((creature (create-creature struct)))
			(let ((type (cons 'npc (@: creature 'type))))
				(box (append (alist-delete 'type creature) (list
					(cons 'type type)
					;... TODO
				)))
			)
		)
	)
	;(define (create-citizen struct)
	;	...
	;)
	;(define (create-monster struct)
	;	...
	;)
	(define (create-character struct)
		(let ((creature (create-creature struct)))
			(let ((type (cons 'character (@: creature 'type))))
				(append (alist-delete 'type creature) (list
					(cons 'type type)
					(cons 'name (@: struct 'name))
					(cons 'title (@: struct 'title))
					(cons 'level (@: struct 'level))
					(cons 'race (@: struct 'race))
					(cons 'gender (@: struct 'gender))
					;(cons 'base-class-id (@: struct 'base-class-id))
					(cons 'clan-id (@: struct 'clan-id))
					(cons 'clothing (@: struct 'clothing))
					(cons 'statements (@: struct 'statements))
				))
			)
		)
	)
	(define (create-antagonist struct)
		(let ((character (create-character struct)))
			(let ((type (cons 'antagonist (@: character 'type))))
				(box (append (alist-delete 'type character) (list
					(cons 'type type)
					; TODO
				)))
			)
		)
	)
	(define (create-protagonist struct)
		(let ((character (create-character struct)))
			(let ((type (cons 'protagonist (@: character 'type))))
				(box (append (alist-delete 'type character) (list
					(cons 'type type)
					(cons 'sp (@: struct 'sp))
					(cons 'xp (@: struct 'xp))
					(cons 'karma (@: struct 'karma))
					(cons 'equipment (@: struct 'equipment))
				)))
			)
		)
	)
	
	(define (update-object object struct)
		(void) ; TODO
	)
	(define (update-creature creature struct)
		(void) ; TODO
	)
	(define (update-npc! npc struct)
		(void) ; TODO
	)
	(define (update-character character struct)
		(void) ; TODO
	)
	(define (update-antagonist! antagonist struct)
		(void) ; TODO
	)
	(define (update-protagonist! protagonist struct)
		(void) ; TODO
	)
	
	(define (object? object)
		(any-is? 'object (@: object 'type))
	)
	(define (creature? object)
		(any-is? 'creature (@: object 'type))
	)
	(define (npc? object)
		(any-is? 'npc (@: object 'type))
	)
	(define (character? object)
		(any-is? 'character (@: object 'type))
	)
	(define (antagonist? object)
		(any-is? 'antagonist (@: object 'type))
	)
	(define (protagonist? object)
		(any-is? 'protagonist (@: object 'type))
	)
	
	(define (creature-angle creature)
		; TODO if creature? and moving? then f(position, destination)
		; TODO else if creature? and casting? then f(position, target.position)
		; TODO else angle
		(@: creature 'angle)
	)
	
	(define (objects-distance a b)
		(distance/3d (@: a 'position) (@: b 'position))
	)
	
	(define (objects-angle a b)
		(let ((pa (@: a 'position)) (pb (@: b 'position)))
			(angle/2d
				(point/2d (point/3d-x pa) (point/3d-y pa))
				(point/2d (point/3d-x pb) (point/3d-y pb))
			)
		)
	)
	
	(define (find-character-by-name world name)
		(hash-find world (lambda (k v)
			(and (integer? k) (character? v) (equal? (@: v 'name) name))
		))
	)
)
