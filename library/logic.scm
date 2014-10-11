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
		(get-object (hash? (or/c integer? symbol?) . -> . box?))
		(find-character-by-name (box? string? . -> . (or/c box? false/c)))
		
		(create-protagonist (list? . -> . box?))
		(create-antagonist (list? . -> . box?))
		(create-npc (list? . -> . box?))
		(update-protagonist! (box? list? . -> . void?))
		(update-antagonist! (box? list? . -> . void?))
		(update-character! (box? list? . -> . void?))
		(update-npc! (box? list? . -> . void?))
		(update-creature! (box? list? . -> . void?))
		
		(object? (box? . -> . boolean?))
		(creature? (box? . -> . boolean?))
		(npc? (box? . -> . boolean?))
		(character? (box? . -> . boolean?))
		(antagonist? (box? . -> . boolean?))
		(protagonist? (box? . -> . boolean?))
		(object=? (object? object? . -> . boolean?))
		
		(points-angle (point/3d? point/3d? . -> . (or/c real? false/c)))
		(points-distance (point/3d? point/3d? . -> . integer?))
		(objects-angle (box? box? . -> . (or/c real? false/c)))
		(objects-distance (box? box? . -> . integer?))
		
		(creature-angle (box? . -> . real?))
		;( ( . -> . ))
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
	
	(define (transfer to from . fields)
		(let ((fields (filter (lambda (i) (assoc i from)) fields))) ; filter exist fields
			(append
				(filter (lambda (i) (member (car i) fields)) from)
				(filter (lambda (i) (not (member (car i) fields))) to)
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
					
					(cons 'name (@: struct 'name))
					(cons 'title (@: struct 'title))
					
					(cons 'hp (@: struct 'hp))
					(cons 'mp (@: struct 'mp))
					(cons 'max-hp (@: struct 'max-hp))
					(cons 'max-mp (@: struct 'max-mp))
					
					(cons 'moving? (@: struct 'moving?))
					
					(cons 'angle (@: struct 'angle))
					(cons 'position (@: struct 'position))
					(cons 'destination (@: struct 'destination))
					(cons 'collision-radius (@: struct 'collision-radius))
					(cons 'collision-height (@: struct 'collision-height))
					
					(cons 'magical-attack-speed (@: struct 'magical-attack-speed))
					(cons 'physical-attack-speed (@: struct 'physical-attack-speed))
					(cons 'move-speed-factor (@: struct 'move-speed-factor))
					(cons 'attack-speed-factor (@: struct 'attack-speed-factor))
					
					(cons 'run-speed (@: struct 'run-speed))
					(cons 'walk-speed (@: struct 'walk-speed))
					(cons 'swim-run-speed (@: struct 'swim-run-speed))
					(cons 'swim-walk-speed (@: struct 'swim-walk-speed))
					(cons 'fly-run-speed (@: struct 'fly-run-speed))
					(cons 'fly-walk-speed (@: struct 'fly-walk-speed))
					
					(cons 'clothing (@: struct 'clothing))
				))
			)
		)
	)
	(define (create-npc struct)
		(let ((creature (create-creature struct)))
			(let ((type (cons 'npc (@: creature 'type))))
				(box (append (alist-delete 'type creature) (list
					(cons 'type type)
					
					(cons 'running? (@: struct 'running?))
					(cons 'in-combat? (@: struct 'in-combat?))
					(cons 'alike-dead? (@: struct 'alike-dead?))
					
					(cons 'attackable? (@: struct 'attackable?))
					(cons 'show-name? (@: struct 'show-name?))
					(cons 'summoned? (@: struct 'summoned?))
				)))
			)
		)
	)
	
	;(define (create-citizen struct) ...)
	;(define (create-monster struct) ...)
	
	(define (create-character struct)
		(let ((creature (create-creature struct)))
			(let ((type (cons 'character (@: creature 'type))))
				(append (alist-delete 'type creature) (list
					(cons 'type type)
					
					(cons 'cp (@: struct 'cp))
					(cons 'max-cp (@: struct 'max-cp))
					(cons 'karma (@: struct 'karma))
					
					(cons 'pvp? (@: struct 'pvp?))
					(cons 'find-party? (@: struct 'find-party?))
					(cons 'hero-icon? (@: struct 'hero-icon?))
					(cons 'hero-glow? (@: struct 'hero-glow?))
					(cons 'fishing? (@: struct 'fishing?))
					(cons 'mount-type (@: struct 'mount-type))
					(cons 'private-store (@: struct 'private-store))
					
					(cons 'race (@: struct 'race))
					(cons 'gender (@: struct 'gender))
					(cons 'face-type (@: struct 'face-type))
					(cons 'hair-style (@: struct 'hair-style))
					(cons 'hair-color (@: struct 'hair-color))
					(cons 'name-color (@: struct 'name-color))
					
					(cons 'class-id (@: struct 'class-id))
					(cons 'clan-id (@: struct 'clan-id))
					(cons 'ally-id (@: struct 'ally-id))
					(cons 'clan-crest-id (@: struct 'clan-crest-id))
					(cons 'ally-crest-id (@: struct 'ally-crest-id))
					
					(cons 'statements (@: struct 'statements))
					(cons 'cubics (@: struct 'cubics))
					(cons 'fish (@: struct 'fish))
				))
			)
		)
	)
	(define (create-antagonist struct)
		(let ((character (create-character struct)))
			(let ((type (cons 'antagonist (@: character 'type))))
				(box (append (alist-delete 'type character) (list
					(cons 'type type)
					
					(cons 'running? (@: struct 'running?))
					(cons 'in-combat? (@: struct 'in-combat?))
					(cons 'alike-dead? (@: struct 'alike-dead?))
					
					(cons 'invisible? (@: struct 'invisible?))
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
					(cons 'level (@: struct 'level))
					(cons 'load (@: struct 'load))
					(cons 'max-load (@: struct 'max-load))

					(cons 'clan-leader? (@: struct 'clan-leader?))
					(cons 'dwarven-craft? (@: struct 'dwarven-craft?))
					
					(cons 'physical-attack-power (@: struct 'physical-attack-power))
					(cons 'physical-defense (@: struct 'physical-defense))
					(cons 'magical-attack-power (@: struct 'magical-attack-power))
					(cons 'magical-defense (@: struct 'magical-defense))
					(cons 'accuracy (@: struct 'accuracy))
					(cons 'evasion (@: struct 'evasion))
					(cons 'focus (@: struct 'focus))
					
					(cons 'pk-count (@: struct 'pk-count))
					(cons 'pvp-count (@: struct 'pvp-count))
					(cons 'inventory-limit (@: struct 'inventory-limit))
					
					(cons 'base-class-id (@: struct 'base-class-id))
					(cons 'access-level (@: struct 'access-level))
					
					(cons 'equipment (@: struct 'equipment))
				)))
			)
		)
	)
	
	(define (update-object object struct)
		(transfer object struct 'object-id)
	)
	
	(define (update-creature creature struct)
		(let ((creature (update-object creature struct)))
			(transfer creature struct
				'name
				'title
				'hp
				'mp
				'max-hp
				'max-mp
				'moving?
				'angle
				'position
				'destination
				'collision-radius
				'collision-height
				'magical-attack-speed
				'physical-attack-speed
				'move-speed-factor
				'attack-speed-factor
				'run-speed
				'walk-speed
				'swim-run-speed
				'swim-walk-speed
				'fly-run-speed
				'fly-walk-speed
				'clothing
			)
		)
	)
	
	(define (update-creature! creature struct)
		(set-box! creature (update-creature (unbox creature) struct))
	)
	
	(define (update-npc! npc struct)
		(set-box! npc
			(let ((npc (update-creature (unbox npc) struct)))
				(transfer npc struct
					; TODO
				)
			)
		)
	)
	(define (update-character character struct)
		(let ((character (update-creature character struct)))
			(transfer character struct
				; TODO
			)
		)
	)
	
	(define (update-character! character struct)
		(set-box! character (update-character (unbox character) struct))
	)
	
	(define (update-antagonist! antagonist struct)
		(set-box! antagonist
			(let ((antagonist (update-character (unbox antagonist) struct)))
				(transfer antagonist struct
					; TODO
				)
			)
		)
	)
	(define (update-protagonist! protagonist struct)
		(set-box! protagonist
			(let ((protagonist (update-character (unbox protagonist) struct)))
				(transfer protagonist struct
					; TODO
				)
			)
		)
	)
	
	(define (object? object)
		(if (member 'object (@: object 'type)) #t #f)
	)
	(define (creature? object)
		(if (member 'creature (@: object 'type)) #t #f)
	)
	(define (npc? object)
		(if (member 'npc (@: object 'type)) #t #f)
	)
	(define (character? object)
		(if (member 'character (@: object 'type)) #t #f)
	)
	(define (antagonist? object)
		(if (member 'antagonist (@: object 'type)) #t #f)
	)
	(define (protagonist? object)
		(if (member 'protagonist (@: object 'type)) #t #f)
	)
	
	(define (object=? a b)
		(= (@: a 'object-id) (@: b 'object-id))
	)
	
	(define (creature-angle creature)
		; TODO if creature? and moving? then f(position, destination)
		; TODO else if creature? and casting? then f(position, target.position)
		; TODO else angle
		(@: creature 'angle)
	)
	
	(define (points-distance a b)
		(round (distance/3d a b))
	)
	
	(define (objects-distance a b)
		(points-distance (@: a 'position) (@: b 'position))
	)
	
	(define (points-angle a b)
		(angle/2d
			(point/2d (point/3d-x a) (- (point/3d-y a))) ; y axis is inverted
			(point/2d (point/3d-x b) (- (point/3d-y b))) ; y axis is inverted
		)
	)
	
	(define (objects-angle a b)
		(points-angle (@: a 'position) (@: b 'position))
	)
	
	(define (find-character-by-name world name)
		(hash-find world (lambda (k v)
			(and (integer? k) (character? v) (equal? (@: v 'name) name))
		))
	)
	
	(define (get-object world id)
		(hash-ref world id #f)
	)
)
