(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
		srfi/1
		"../library/structure.scm"
		"main.scm"
		"object.scm"
	)
	(provide (contract-out
		(creature? (box? . -> . boolean?))
		(create-creature (list? . -> . list?))
		(update-creature (list? list? . -> . list?))
		(update-creature! (box? list? . -> . void?))
		(creature-angle (box? . -> . real?))
	))

	(define (creature? object)
		(if (member 'creature (@: object 'type)) #t #f)
	)

	(define (create-creature struct)
		(let ((object (create-object struct)))
			(let ((type (cons 'creature (@: object 'type))))
				(append (alist-delete 'type object) (list
					(cons 'type type)
					
					(cons 'name (@: struct 'name))
					(cons 'title (@: struct 'title))
					
					(cons 'target-id #f)
					
					(cons 'hp (@: struct 'hp))
					(cons 'mp (@: struct 'mp))
					(cons 'max-hp (@: struct 'max-hp))
					(cons 'max-mp (@: struct 'max-mp))
					
					(cons 'moving? (@: struct 'moving?))
					(cons 'running? (@: struct 'running?))
					(cons 'in-combat? (@: struct 'in-combat?))
					(cons 'alike-dead? (@: struct 'alike-dead?))
					
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

	(define (update-creature creature struct)
		(let ((creature (update-object creature struct)))
			(struct-transfer creature struct
				'name
				'title
				'hp
				'mp
				'max-hp
				'max-mp
				'moving?
				'running?
				'in-combat?
				'alike-dead?
				'target-id
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
	
	(define (creature-angle creature)
		; TODO if creature? and moving? then f(position, destination)
		; TODO else if creature? and casting? then f(position, target.position)
		; TODO else angle
		(@: creature 'angle)
	)
)