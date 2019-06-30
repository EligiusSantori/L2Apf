(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
		srfi/1
		"../library/structure.scm"
		"creature.scm"
	)
	(provide (contract-out
		(character? (any/c . -> . boolean?))
		(create-character (list? . -> . list?))
		(update-character (list? list? . -> . list?))
		(update-character! (box? list? . -> . void?))
	))

	(define (character? object)
		(if (and object (member 'character (@: object 'type))) #t #f)
	)

	(define (create-character struct)
		(let ((creature (create-creature struct)))
			(let ((type (cons 'character (@: creature 'type))))
				(append (alist-delete 'type creature) (list
					(cons 'type type)

					; TODO level (exists in party packet)

					(cons 'cp (@: struct 'cp))
					(cons 'max-cp (@: struct 'max-cp))
					(cons 'karma (@: struct 'karma))

					(cons 'pvp? (@: struct 'pvp?))
					(cons 'invisible? (@: struct 'invisible?))
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
					(cons 'cubics (@: struct 'cubics))
					(cons 'fish (@: struct 'fish))
				))
			)
		)
	)

	(define (update-character character struct)
		(let ((character (update-creature character struct)))
			(struct-transfer character struct
				'cp
				'max-cp
				'karma
				'pvp?
				'invisible?
				'find-party?
				'hero-icon?
				'hero-glow?
				'fishing?
				'mount-type
				'private-store
				'race
				'gender
				'face-type
				'hair-style
				'hair-color
				'name-color
				'class-id
				'clan-id
				'ally-id
				'clan-crest-id
				'ally-crest-id
				'cubics
				'fish
			)
		)
	)

	(define (update-character! character struct)
		(set-box! character (update-character (unbox character) struct))
	)
)
