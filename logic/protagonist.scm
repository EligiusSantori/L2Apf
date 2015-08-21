(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
		srfi/1
		"../library/structure.scm"
		"main.scm"
		"character.scm"
	)
	(provide (contract-out
		(protagonist? (box? . -> . boolean?))
		(create-protagonist (list? . -> . box?))
		(update-protagonist! (box? list? . -> . void?))
	))

	(define (protagonist? object)
		(if (member 'protagonist (@: object 'type)) #t #f)
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

					(cons 'died? #f)
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
					
					(cons 'statements (@: struct 'statements))
					(cons 'equipment (@: struct 'equipment))
				)))
			)
		)
	)
	
	(define (update-protagonist protagonist struct)
		(let ((protagonist (update-character protagonist struct)))
			(struct-transfer protagonist struct
				'sp
				'xp
				'level
				'load
				'max-load
				'died?
				'clan-leader?
				'dwarven-craft?
				'physical-attack-power
				'physical-defense
				'magical-attack-power
				'magical-defense
				'accuracy
				'evasion
				'focus
				'pk-count
				'pvp-count
				'inventory-limit
				'base-class-id
				'access-level
				'statements
				'equipment
			)
		)
	)
	
	(define (update-protagonist! protagonist struct)
		(set-box! protagonist (update-protagonist (unbox protagonist) struct))
	)
)