(module logic racket/base
	(require
		srfi/1
		(only-in racket/function negate)
		(rename-in racket/contract (any all/c))
		"../library/geometry.scm"
		"../system/structure.scm"
		"creature.scm"
	)
	(provide (contract-out
		(character? (-> any/c boolean?))
		(make-character (-> list? list?))
		(update-character (-> list? list? (values list? list? list?)))
		(update-character! (-> box? list? list?))

		(find-class-name (->* (integer?) (list?) (or/c symbol? false/c)))
		(fighter-type? (-> character? boolean?))
		(mystic-type? (-> character? boolean?))
		(wizard-class? (-> character? boolean?))
		(support-class? (-> character? boolean?))
		(summoner-class? (-> character? boolean?))
	))

	(define character (list
		(cons 'level (negate =))
		(cons 'cp (negate =))
		(cons 'max-cp (negate =))
		(cons 'karma (negate =))

		(cons 'race (negate =))
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
		(cons 'fish (negate point/3d=))
	))

	(define (character? object)
		(if (and object (member 'character (ref object 'type))) #t #f)
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
	(define classes (list
		(list -1 'figter (list
			(list 0 'human-fighter (list
				(list 1 'warrior (list
					(list 2 'gladiator (list (list 88 'duelist)))
					(list 3 'warlord (list (list 89 'dreadnought)))
				))
				(list 4 'human-knight (list
					(list 5 'paladin (list (list 90 'phoenix-knight)))
					(list 6 'dark-avenger (list (list 91 'hell-knight)))
				))
				(list 7 'rogue  (list
					(list 8 'treasure-hunter (list (list 93 'adventurer)))
					(list 9 'hawkeye (list (list 92 'sagittarius)))
				))
			))
			(list 18 'elf-fighter (list
				(list 19 'elf-knight (list
					(list 20 'temple-knight (list (list 99 'evas-templar)))
					(list 21 'sword-singer (list (list 100 'sword-muse)))
				))
				(list 22 'elven-scout (list
					(list 23 'plains-walker (list (list 101 'wind-rider)))
					(list 24 'silver-ranger (list (list 102 'moonlight-sentinel)))
				))
			))
			(list 31 'de-fighter (list
				(list 32 'palus-knight (list
					(list 33 'shillien-knight (list (list 106 'shillien-templar)))
					(list 34 'bladedancer (list (list 107 'spectral-dancer)))
				))
				(list 35 'assassin (list
					(list 36 'abyss-walker (list (list 108 'ghost-hunter)))
					(list 37 'phantom-ranger (list (list 109 'ghost-sentinel)))
				))
			))
			(list 44 'orc-fighter (list
				(list 45 'orc-raider (list
					(list 46 'destroyer (list (list 113 'titan)))
				))
				(list 47 'orc-monk (list
					(list 48 'tyrant (list (list 114 'grand-khavatari)))
				))
			))
			(list 53 'dwarven-fighter (list
				(list 54 'scavenger (list
					(list 55 'bounty-hunter (list (list 117 'fortune-seeker)))
				))
				(list 56 'artisan (list
					(list 57 'warsmith (list (list 118 'maestro)))
				))
			))
		))
		(list -2 'mystic (list
			(list 10 'human-mystic (list
				(list 11 'human-wizard (list
					(list 12 'sorcerer (list (list 94 'archmage)))
					(list 13 'necromancer (list (list 95 'soultaker)))
					(list 14 'warlock (list (list 96 'arcana-lord)))
				))
				(list 15 'cleric (list
					(list 16 'bishop (list (list 97 'cardinal)))
					(list 17 'prophet (list (list 98 'hierophant)))
				))
			))
			(list 25 'elven-mystic (list
				(list 26 'elven-wizard (list
					(list 27 'spellsinger (list (list 103 'mystic-muse)))
					(list 28 'elemental-summoner (list (list 104 'elemental-master)))
				))
				(list 29 'elven-oracle (list
					(list 30 'elven-elder (list (list 105 'evas-saint)))
				))
			))
			(list 38 'de-mystic (list
				(list 39 'de-wizard (list
					(list 40 'spellhowler (list (list 110 'storm-screamer)))
					(list 41 'phantom-summoner (list (list 111 'spectral-master)))
				))
				(list 42 'shillien-oracle (list
					(list 43 'shillien-elder (list (list 112 'shillien-saint)))
				))
			))
			(list 49 'orc-mystic (list
				(list 50 'orc-shaman (list
					(list 51 'overlord (list (list 115 'dominator)))
					(list 52 'warcryer (list (list 116 'doomcryer)))
				))
			))
		))
	))
	(define (find-class-name class-id [branch classes])
		(let ((node (car branch)) (rest (cdr branch)))
			(let ((id (car node)) (name (cadr node)) (childs (cddr node)))
				(if (= id class-id)
					name ; Value found.
					(if (not (null? childs))
						(or
							(find-class-name class-id (car childs)) ; Go depth.
							(and (not (null? rest))
								(find-class-name class-id rest)  ; Go next.
							)
						)
						(and (not (null? rest))
							(find-class-name class-id rest)  ; Go next.
						)
					)
				)
			)
		)
	)
	(define (fighter-type? character)
		(if (find-class-name (ref character 'class-id) (caddr (car classes))) #t #f)
	)
	(define (mystic-type? character)
		(if (find-class-name (ref character 'class-id) (caddr (cadr classes))) #t #f)
	)
	(define (wizard-class? character)
		(let ((class (find-class-name (ref character 'class-id) classes)))
			(if (member class (list
				'sorcerer 'archmage
				'spellsinger 'mystic-muse
				'spellhowler 'storm-screamer
			)) #t #f)
		)
	)
	(define (support-class? character)
		(let ((class (find-class-name (ref character 'class-id) classes)))
			(if (member class (list
				'cleric
					'bishop 'cardinal
					'prophet 'hierophant
				'elven-oracle 'elven-elder 'evas-saint
				'shillien-oracle 'shillien-elder 'shillien-saint
			)) #t #f)
		)
	)
	(define (summoner-class? character)
		(let ((class (find-class-name (ref character 'class-id) classes)))
			(if (member class (list
				'warlock 'arcana-lord
				'elemental-summoner 'elemental-master
				'phantom-summoner 'spectral-master
			)) #t #f)
		)
	)
)
