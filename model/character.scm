(module logic racket/base
	(require
		(only-in srfi/1 fold alist-delete)
		(only-in racket/function negate)
		racket/contract
		"../library/geometry.scm"
		"../packet/game/class.scm"
		"../system/structure.scm"
		"object.scm"
		"creature.scm"
	)
	(provide (contract-out
		(character? (-> any/c boolean?))
		(make-character (-> list? list?))
		(update-character (-> list? list? (values list? list? list?)))
		(update-character! (-> box? list? list?))

		(fighter-type? (-> character? boolean?))
		(mystic-type? (-> character? boolean?))
		(wizard-class? (-> character? any))
		(support-class? (-> character? any))
		(recharger-class? (-> character? any))
		(summoner-class? (-> character? any))
		(artisan-class? (-> character? any))
		(scavenger-class? (-> character? any))
	))

	(define character (list
		(cons 'level (negate =))
		(cons 'cp (negate =))
		(cons 'max-cp (negate =))
		(cons 'karma (negate =))

		(cons 'race (negate eq?))
		(cons 'gender (negate eq?))
		(cons 'face-type (negate =))
		(cons 'hair-style (negate =))
		(cons 'hair-color (negate =))
		(cons 'name-color (negate =))
		(cons 'class-id (negate =))

		(cons 'pvp? (negate eq?))
		(cons 'invisible? (negate eq?))
		(cons 'find-party? (negate eq?))
		(cons 'hero-icon? (negate eq?))
		(cons 'hero-glow? (negate eq?))
		(cons 'fishing? (negate eq?))
		(cons 'mount-type (negate eq?))
		(cons 'private-store (negate eq?))

		(cons 'clan-id (negate =))
		(cons 'clan-crest-id (negate =))
		(cons 'ally-id (negate =))
		(cons 'ally-crest-id (negate =))

		(cons 'cubics (negate equal?)) ; TODO use set
		(cons 'fish (negate point/3d=?))
	))

	(define (character? object)
		(object-of-type? object 'character)
	)

	(define (make-character data)
		(let ((creature (make-creature data)))
			(let ((type (cons 'character (ref creature 'type))))
				(fold
					(lambda (p r) (if (and p (assoc (car p) character eq?)) (cons p r) r)) ; If field belongs to character.
					(cons (cons 'type type) (alist-delete 'type creature))
					data
				)
			)
		)
	)

	(define (update-character object data)
		(let-values (((rest updated changes) (update-creature object data)))
			(struct-update data character rest updated changes)
		)
	)
	(define (update-character! object data)
		(let-values (((rest updated changes) (update-character (unbox object) data)))
			(set-box! object (append rest updated))
			changes
		)
	)

	; TODO Use shared database.
	(define (fighter-type? character)
		(if (find-class-name (ref character 'class-id) (caddr (car classes))) #t #f)
	)
	(define (mystic-type? character)
		(if (find-class-name (ref character 'class-id) (caddr (cadr classes))) #t #f)
	)
	(define (wizard-class? character)
		(let ((class (find-class-name (ref character 'class-id))))
			(and (member class (list
				'sorcerer 'archmage
				'spellsinger 'mystic-muse
				'spellhowler 'storm-screamer
			)) class)
		)
	)
	(define (support-class? character)
		(let ((class (find-class-name (ref character 'class-id))))
			(and (member class (list
				'cleric
					'bishop 'cardinal
					'prophet 'hierophant
				'elven-oracle 'elven-elder 'evas-saint
				'shillien-oracle 'shillien-elder 'shillien-saint
			)) class)
		)
	)
	(define (recharger-class? character)
		(let ((class (support-class? character)))
			(and class (member class (list
				'elven-oracle 'elven-elder 'evas-saint
				'shillien-oracle 'shillien-elder 'shillien-saint
			)) class)
		)
	)
	(define (summoner-class? character)
		(let ((class (find-class-name (ref character 'class-id))))
			(and (member class (list
				'warlock 'arcana-lord
				'elemental-summoner 'elemental-master
				'phantom-summoner 'spectral-master
			)) class)
		)
	)
	(define (artisan-class? character)
		(let ((class (find-class-name (ref character 'class-id))))
			(and (member class (list 'artisan 'warsmith 'maestro)) class)
		)
	)
	(define (scavenger-class? character)
		(let ((class (find-class-name (ref character 'class-id))))
			(and (member class (list 'scavenger 'bounty-hunter 'fortune-seeker)) class)
		)
	)
)
