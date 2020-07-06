(module logic racket/base
	(require
		(only-in srfi/1 fold alist-delete)
		(only-in racket/function negate)
		racket/contract
		; "../library/extension.scm"
		"../library/geometry.scm"
		"../system/structure.scm"
		"../system/database.scm"
		"object.scm"
		"creature.scm"
	)
	(provide (contract-out
		(character? (-> any/c boolean?))
		(make-character (-> list? any/c list?))
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
		(tank-class? (-> character? any))
	))

	(define character (list
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

		(cons 'class #f)
		(cons 'specialty #f)
		(cons 'mystic? #f)
	))

	(define (character? object)
		(object-of-type? object 'character)
	)

	(define (make-character data db) ; TODO Can be optimized.
		(let* ((creature (make-creature data)) (type (cons 'character (ref creature 'type))))
			(append
				(list (cons 'type type))
				(db-character db (ref data 'class-id))
				(fold ; TODO extract class-id
					(lambda (p r) (if (and p (assoc (car p) character eq?)) (cons p r) r)) ; If field belongs to character.
					(alist-delete 'type creature eq?) ; TODO extract type
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
		(if (ref character 'mystic?) #f #t)
	)
	(define (mystic-type? character)
		(if (ref character 'mystic?) #t #f)
	)
	(define (wizard-class? character)
		(let ((class (ref character 'class)))
			(and (member class (list
				'sorcerer 'archmage
				'spellsinger 'mystic-muse
				'spellhowler 'storm-screamer
			)) class)
		)
	)
	(define (support-class? character)
		(let ((class (ref character 'class)))
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
		(let ((class (ref character 'class)))
			(and (member class (list
				'warlock 'arcana-lord
				'elemental-summoner 'elemental-master
				'phantom-summoner 'spectral-master
			)) class)
		)
	)
	(define (artisan-class? character)
		(let ((class (ref character 'class)))
			(and (member class (list 'artisan 'warsmith 'maestro)) class)
		)
	)
	(define (scavenger-class? character)
		(let ((class (ref character 'class)))
			(and (member class (list 'scavenger 'bounty-hunter 'fortune-seeker)) class)
		)
	)
	(define (tank-class? character)
		(let ((class (ref character 'class)))
			(and (member class (list
				'human-knight
					'paladin 'phoenix-knight
					'dark-avenger 'hell-knight
				'temple-knight
				'shillien-knight
			)) class)
		)
	)
)
