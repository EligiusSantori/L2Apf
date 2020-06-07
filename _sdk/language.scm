(module script racket/base
	(require
		(only-in srfi/1 fold)
		(only-in racket/list remove-duplicates)
		racket/string
		(only-in racket/function const)
		(only-in racket/format ~r)
		(relative-in "../."
			"library/extension.scm"
			"system/structure.scm"
			"system/connection.scm"
			"model/map.scm"
			"model/skill.scm"
			"model/object.scm"
			"model/item.scm"
			"model/inventory.scm"
			"model/creature.scm"
			"model/character.scm"
			"model/protagonist.scm"
			"model/party.scm"
			"model/npc.scm"
			"model/world.scm"
			"api/say.scm"
			"api/pick_up.scm"
			"api/use_item.scm"
			"api/drop.scm"
			"api/target.scm"
			"api/use_skill.scm"
			(only-in "program/program.scm" program)
			"program/slay.scm"
			"program/bless.scm"
			"program/brain.scm"
		)
	)
	(provide
		parse-command
		; parse-targets
		command-show
		command-use
		command-drop
		command-pick
		command-skill
		command-assist
		command-buff
	)

	(define (parse-command wr text channel author-id) ; TODO parse(command) e.g. (parse "follow [me]|my|<name>") ; string -> verb, [noun], [noun...]
		(if (or (eq? channel 'chat-channel/tell) (and (eq? channel 'chat-channel/party) (eq? author-id (party-leader (world-party wr)))))
			(filter (compose not zero? string-length) (string-split (string-downcase text) " "))
			#f
		)
	)

	(define (parse-targets wr author arguments [default #f])
		(if (or (not (null? arguments)) default)
			(remove-duplicates (fold (lambda (argument result)
				(case argument
					(("me") (cons (object-id author) result))
					(("self") (cons (object-id (world-me wr)) result))
					(("target") (let ((target-id (ref author 'target-id))) (displayln target-id) (if target-id (cons target-id result) result)))
					(("party") (append (party-members (world-party wr)) result))
					(else (let ((character (find-character wr argument))) (if character (cons (object-id character) result) result)))
				)
			) (list) (if (null? arguments) (list default) arguments)) =)
			(list)
		)
	)

	(define (command-show cn arguments)
		(let ((me (world-me (connection-world cn))) (what (try-first arguments))) (case what
			(("level") (say cn (format "Level: ~a." (~r (get-level me) #:precision 2)) 'chat-channel/party))
			(("sp") (say cn (format "SP: ~a." (ref me 'sp)) 'chat-channel/party))
		))
	)

	(define (items-by-name wr name)
		(let ((inv (world-inventory wr)))
			(case name
				(("soe") (find-items inv 736 1829 1830))
				(("a") (find-items inv 57))
				(("aa") (find-items inv 5575 6360 6361 6362))
				(else (list))
			)
		)
	)
	(define (parse-items wr what)
		(let ((item-id (string->number what)))
			(if item-id
				(find-items (world-inventory wr) item-id)
				(items-by-name wr what)
			)
		)
	)
	(define (command-use cn arguments)
		(let ((what (try-first arguments))) (when what
			(let ((item (try-first (parse-items (connection-world cn) what))))
				(when item (use-item cn (object-id item)))
			)
		))
	)
	(define (command-drop cn arguments)
		(let ((what (try-first arguments))) (when what
			(let ((items (parse-items (connection-world cn) what)))
				(when (not (null? items))
					(map (lambda (item)
						(drop cn (object-id item) (ref item 'count)) ; TODO position + offset (forward).
						(sleep 1) ; TODO use timer
					) items)
				)
			)
		))
	)

	(define (closest-item wr)
		(let ((position (get-position (world-me wr))))
			(cdr (fold-objects wr (cons #f #f) (lambda (object r)
				(if (and (item? object) (on-ground? object))
					(let ((distance (points-distance (ref object 'position) position)))
						(if (or (not (car r)) (< distance (car r)))
							(cons distance object)
							r
						)
					)
					r
				)
			)))
		)
	)
	(define (command-pick cn)
		(let ((item (closest-item (connection-world cn))))
			(when (item) (pick-up cn (object-id item)))
		)
	)

	(define (command-skill cn arguments)
		(let ((what (try-first arguments))) (case what
			(("vic") (use-skill cn (car (select-skills 'vicious-stance))))
			(("hate") (use-skill cn (car (select-skills 'hate))))
			(else (let ((skill-id (string->number what)))
				(when skill-id (use-skill cn skill-id))
			))
		))
	)

	(define (skill-exists wr name)
		(skill-ref wr (car (select-skills name)))
	)
	(define (skill-settings wr)
		(list
			(cons 'power-strike (lambda (me target . rest) (and
				(equip-sword? me)
				(> (mp-ratio me) 1/3)
				(or
					(boss? target) (minion? target)
					(and
						(not (skill-exists wr 'shield-stun))
						(>= (or (ref target 'hp) 0) (ref me 'hp)) ; Target.HP > Me.HP
					)
				)
				; TODO not tank
			)))

			(cons 'mortal-blow (lambda (me target . rest) (and
				(equip-dagger? me)
				(> (mp-ratio me) 1/3)
				(or
					(boss? target) (minion? target)
					(>= (or (ref target 'hp) 0) (ref me 'hp)) ; Target.HP > Me.HP
				)
			)))

			#|(cons 'power-shot (lambda (me target . rest) (and
				(equip-bow? me)
				(> (mp-ratio me) 2/3)
				(> (hp-ratio target) 1/3)
			)))|#

			(cons 'wild-sweep (lambda (me target . rest) (and
				(equip-spear? me)
				(> (mp-ratio me) 1/3)
				(if (or (boss? target) (minion? target))
					(not (skill-exists wr 'vicious-stance))
					(and
						(not (skill-exists wr 'spoil))
						(>= (or (ref target 'hp) 0) (ref me 'hp)) ; Target.HP > Me.HP
					)
				)
			)))

			(cons 'shield-stun (lambda (me target . rest) (and
				(equip-shield? me)
				(> (mp-ratio me) 1/3)
				(not (ref target 'stunned?))
				(>= (or (ref target 'hp) 0) (* (ref me 'hp) 2)) ; Target.HP > (Me.HP * 2)
				; TODO not tank
			)))

			(cons 'poison (lambda (me target . rest) (and
				(> (mp-ratio me) 1/5)
				(not (ref target 'poisoned?))
				(not (world-party wr)) ; Too slow casting for party.
				(not (or (boss? target) (minion? target))) ; Too slow casting & too small chances for raid.
				(>= (or (ref target 'hp) 0) (ref me 'hp)) ; Target.HP > Me.HP
			)))

			(cons 'sting (lambda (me target . rest) (and
				(or (equip-duals? me) (equip-sword? me) (equip-dagger? me))
				(> (mp-ratio me) 1/5)
				(not (ref target 'bleeding?))
				(or
					(boss? target) (minion? target)
					(>= (or (ref target 'hp) 0) (* (ref me 'hp) 2)) ; Target.HP > (Me.HP * 2)
				)
			)))

			(cons 'drain-health  (lambda (me target . rest) (and
				(<= (hp-ratio me) 1/5)
				(>= (mp-ratio me) 1/5)
				(not (or (boss? target) (minion? target))) ; Ineffective for targets with high physical defense.
			)))

			(cons 'elemental-heal  (lambda (me . rest) (and
				(<= (hp-ratio me) 1/5)
				(>= (mp-ratio me) 1/5)
			)))

			(cons 'spoil (lambda (me target . rest) (and
				(> (mp-ratio me) 1/9)
				(npc? target)
				(not (or (boss? target) (minion? target)))
				(not (ref target 'spoiled?))
			)))

			(cons 'war-cry (lambda (me target . rest) (and
				(>= (mp-ratio me) 1/10)
				(or (boss? target) (minion? target))
			)))

			(cons 'ultimate-defense  (lambda (me . rest) (and
				(<= (hp-ratio me) 1/10)
				(>= (mp-ratio me) 1/10)
			)))
		)
	)
	(define (command-assist cn br author-id skills)
		(let* ((wr (connection-world cn)) (author (object-ref wr author-id)))
			(if author
				(let ((target-id (ref author 'target-id)))
					(if (fighter-type? (world-me wr))
						(when (and target-id (not (in-party? (world-party wr) target-id)))
							(brain-do! br (program program-slay target-id (append (skill-settings wr) skills)))
						)
						(target cn target-id)
					)
				)
				(say cn "Don't see the requester.")
			)
		)
	)

	(define (parse-buffs pack)
		(case pack
			(("ww") (const (list 'wind-walk)))
			(("acum") (const (list 'acumen)))
			(("mgr") (lambda (character)
				(if (fighter-type? character)
					(list 'wind-walk 'vampiric-rage 'shield)
					; (list 'vampiric-rage 'shield 'guidance)
					(list 'wind-walk)
				)
			))
			(("rb") (lambda (character)
				(cond
					((protagonist? character) (list 'acumen))
					((fighter-type? character) (list 'vampiric-rage 'shield 'guidance #|'death-whisper 'focus 'might|#))
					((support-class? character) (list 'wind-walk 'acumen))
					((wizard-class? character) (list 'empower 'acumen))
					(else (list))
				)
			))
			(else (const (list)))
		)
		#|(define (estimate character) (cond ; Support-party.
			((protagonist? character) (list)) ; No buff for myself.
			((string-ci=? (ref character 'name) "Fury") (list 'vampiric-rage 'death-whisper 'might 'guidance)) ; C-grade penalty.
			((mystic-type? character) (list 'concentration 'wind-walk))
			(else (list 'vampiric-rage 'death-whisper 'might))
		))|#
	)
	(define (command-buff cn br author-id arguments)
		(let* ((wr (connection-world cn)) (me (world-me wr)))
			(when (and (support-class? me) (not (null? arguments)))
				(let ((author (object-ref wr author-id)))
					(if author
						(brain-do! br (program program-bless
							(parse-targets wr author (cdr arguments) "target")
							(parse-buffs (car arguments))
						))
						(say cn "Don't see the requester.")
					)
				)
			)
		)
	)
)
