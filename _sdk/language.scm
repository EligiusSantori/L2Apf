(module script racket/base
	(require
		(only-in srfi/1 fold)
		(only-in racket/list remove-duplicates)
		racket/string
		(only-in racket/function const)
		(only-in racket/format ~r)
		(relative-in "../."
			"library/extension.scm"
			"library/date_time.scm"
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
			"api/move_to.scm"
			"api/pick_up.scm"
			"api/use_item.scm"
			"api/drop.scm"
			"api/target.scm"
			"api/attack.scm"
			"api/use_skill.scm"
			(only-in "program/program.scm" program)
			"program/slay.scm"
			"program/bless.scm"
			"program/drop.scm"
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
		command-repeat
		command-assist
		command-buff
	)

	(define (parse-command wr text channel author-id) ; TODO parse(command) e.g. (parse "follow [me]|my|<name>") ; string -> verb, [noun], [noun...]
		(if (or
				(member channel (list 'chat-channel/tell 'chat-channel/clan 'chat-channel/trade) eq?)
				(and (eq? channel 'chat-channel/party) (eq? author-id (party-leader (world-party wr)))))
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
					(("target") (let ((target-id (ref author 'target-id)))
					 	(if target-id (cons target-id result) result)
					))
					(("party") (append (party-members (world-party wr)) result))
					(("us") (let ((me (world-me wr)))
						(fold-objects wr (list) (lambda (object r)
							(if (and (character? object) (not (object=? object me)) (<= (creatures-distance object me) 500))
								(cons (object-id object) r)
								r
							)
						))
					))
					; TODO clan
					(else (let ((character (find-character wr argument)))
						(if character (cons (object-id character) result) result)
					))
				)
			) (list) (if (null? arguments) (list default) arguments)) =)
			(list)
		)
	)

	(define (command-show cn arguments)
		(let ((me (world-me (connection-world cn))) (what (try-first arguments))) (case what
			(("level") (say cn (format "Level: ~a." (~r (ref me 'level) #:precision (list '= 2)))))
			(("sp") (say cn (format "SP: ~a." (ref me 'sp))))
			(("hp") (say cn (format "HP: ~a%." (round (* (hp-ratio me) 100)))))
			(("mp") (say cn (format "MP: ~a%." (round (* (mp-ratio me) 100)))))
		))
	)

	(define (items-by-name wr name)
		(let ((inv (world-inventory wr)))
			(case name
				(("soe") (find-items inv 736 1829 1830))
				(("a") (find-items inv 57))
				(("aa") (find-items inv 5575 6360 6361 6362))
				(("antidote") (find-items inv 1831 1832))
				(("bandage") (find-items inv 1833 1834))
				(("heal") (find-items inv 1060 1061))
				(("haste") (find-items inv 735))
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
	(define (drop-selector wr what)
		(case what
			(("all") (let ((me (world-me wr)))
				(lambda (item) (and
					(not (member (ref item 'item-id) (list 57 5575 6360 6361 6362) =))
					(not (equipped? me (object-id item)))
				))
			))
			(else (let ((items (parse-items wr what)))
				(lambda (item) (member item items object=?))
			))
		)
	)
	(define (command-drop cn br arguments)
		(let ((what (try-first arguments))) (when what
			(brain-do! br (program program-drop (drop-selector (connection-world cn) what)) #t)
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
			(when item (pick-up cn (object-id item)))
		)
	)

	(define (command-skill cn arguments)
		(let ((what (try-first arguments)) (wr (connection-world cn)))
			(case what
				(("arr") (use-skill cn (find-skill wr 'deflect-arrow)))

				(("lif") (use-skill cn (find-skill wr 'chant-of-life)))

				(("con") (use-skill cn (find-skill wr 'confusion)))
				(("pic") (use-skill cn (find-skill wr 'charm)))
				(("mad") (use-skill cn (find-skill wr 'madness)))
				(else (let ((skill-id (string->number what)))
					(when skill-id (use-skill cn (skill-ref wr skill-id)))
				))
			)
		)
	)
	(define (command-repeat cn br skill author-id arguments)
		(let* ((wr (connection-world cn)) (author (object-ref wr author-id)))
			(when author
				(let ((targets (parse-targets wr author arguments "target")))
					(if (not (null? targets))
						(brain-do! br (program program-slay (car targets) (list
							(cons skill (lambda (me . rest) (> (mp-ratio me) 1/10)))
						) void))
						(say cn "Target not found.")
					)
				)
			)
		)
	)

	(define (skill-exists wr name) (find-skill wr name))
	(define (medium-damage-skill-pointful? me target)
		(>= (or (ref target 'hp) 0) (* (ref me 'hp) 2/3)) ; Target.HP > (2/3)Me.HP
	)
	(define (high-damage-skill-pointful? me target)
		(>= (or (ref target 'hp) 0) (ref me 'hp)) ; Target.HP > Me.HP
	)
	(define (stun-skill-pointful? me target) ; Party-tuned. ; TODO Target.HP > (1/5)Party.HP
		(and
			(not (ref target 'stunned?))
			(not (or (ref target 'boss?) (ref target 'minion?))) ; Won't work on boss.
			(>= (or (ref target 'hp) 0) (* (ref me 'hp) 2)) ; Target.HP > (2)Me.HP
		)
	)
	(define (fast-decay-skill-pointful? me target) ; Party-tuned. ; TODO Target.HP > (1/3)Party.HP or boss?
		(>= (or (ref target 'hp) 0) (* (ref me 'hp) 2)) ; Target.HP > (2)Me.HP
	)
	(define (slow-decay-skill-pointful? me target) ; Party-tuned. ; TODO Target.HP > (1/2)Party.HP or boss?
		(>= (or (ref target 'hp) 0) (* (ref me 'hp) 3)) ; Target.HP > (3)Me.HP
	)
	(define (mp-plenty? me) (> (mp-ratio me) 2/3))
	(define (mp>reserve? me) (> (mp-ratio me) 1/3))
	(define (mp>shortage? me) (>= (mp-ratio me) 1/5))
	(define (hp-danger? me) (<= (hp-ratio me) 1/5))
	(define (hp-critical? me) (<= (hp-ratio me) 1/10))
	(define (skill-settings wr)
		(list
			(cons 'wind-strike (lambda (me target skill) (and
				(< (ref me 'specialty) 1)
				(medium-damage-skill-pointful? me target)
				(>= (- (timestamp) (or (ref skill 'last-usage) 0)) 7) ; Delay 7s.
			)))

			(cons 'power-strike (lambda (me target . rest) (and
				(< (ref me 'specialty) 1)
				(equip-sword? me)
				(mp>reserve? me)
				(medium-damage-skill-pointful? me target)
			)))
			(cons 'power-smash (lambda (me target . rest) (and
				(equip-sword? me)
				(mp>reserve? me)
				(if (or (ref target 'boss?) (ref target 'minion?))
					(not (skill-exists wr 'vicious-stance))
					(medium-damage-skill-pointful? me target)
				)
			)))
			(cons 'mortal-blow (lambda (me target . rest) (and
				(equip-dagger? me)
				(mp>reserve? me)
				(if (or (ref target 'boss?) (ref target 'minion?))
					(not (skill-exists wr 'vicious-stance))
					(high-damage-skill-pointful? me target)
				)
			)))
			(cons 'iron-punch (lambda (me target . rest) (and
				(equip-fists? me)
				(mp>reserve? me)
				(medium-damage-skill-pointful? me target)
			)))
			#|(cons 'power-shot (lambda (me target . rest) (and
				(equip-bow? me)
				(mp-plenty? me)
				(medium-damage-skill-fit? me target)
			)))|#
			(cons 'wild-sweep (lambda (me target . rest) (and
				(equip-spear? me)
				(mp>reserve? me)
				(if (or (ref target 'boss?) (ref target 'minion?))
					(and
						(not (skill-exists wr 'vicious-stance))
						(not (skill-exists wr 'whirlwind))
					)
					(and
						(not (skill-exists wr 'spoil))
						(medium-damage-skill-pointful? me target)
					)
				)
			)))
			(cons 'whirlwind (lambda (me target . rest) (and
				(equip-spear? me)
				(mp>reserve? me)
				(if (or (ref target 'boss?) (ref target 'minion?))
					(not (skill-exists wr 'vicious-stance))
					(and
						(not (skill-exists wr 'spoil))
						(medium-damage-skill-pointful? me target)
					)
				)
			)))
			(cons 'stunning-fist (lambda (me target . rest) (and
				(equip-fists? me)
				(mp>reserve? me)
				(stun-skill-pointful? me target)
			)))
			(cons 'shield-stun (lambda (me target . rest) (and
				(equip-shield? me)
				(mp>reserve? me)
				(stun-skill-pointful? me target)
			)))
			(cons 'sting (lambda (me target . rest) (and
				(or (equip-duals? me) (equip-sword? me) (equip-dagger? me))
				(mp>shortage? me)
				(not (ref target 'bleeding?))
				(if (or (ref target 'boss?) (ref target 'minion?))
					(not (skill-exists wr 'vicious-stance))
					(fast-decay-skill-pointful? me target)
				)
			)))

			(cons 'poison (lambda (me target . rest) (and
				(mp>shortage? me)
				(not (ref target 'poisoned?))
				; (slow-decay-skill-pointful? me target)
				(not (world-party wr)) ; Too slow casting for party.
				(not (or (ref target 'boss?) (ref target 'minion?))) ; Too slow casting & too small chances for raid.
				(>= (or (ref target 'hp) 0) (ref me 'hp)) ; Target.HP > Me.HP
			)))
			#|(cons 'frost-flame (lambda (me target . rest) (and
				(mp>reserve? me) ; ? (mp-plenty? me)
				(not (ref target 'bleeding?)) ; (not (ref target 'burning?)) ; Bug?
				; (fast-decay-skill-pointful? me target)
				(or
					(ref target 'boss?) (ref target 'minion?)
					(not (world-party wr)) ; Too slow casting for party.
				)
			)))
			(cons 'venom (lambda (me target . rest) (and
				(mp>reserve? me) ; ? (mp-plenty? me)
				(not (ref target 'poisoned?))
				; (slow-decay-skill-pointful? me target)
				(or
					(ref target 'boss?) (ref target 'minion?)
					(not (world-party wr)) ; Too slow casting for party.
				)
			)))|#
			(cons 'aura-sink (lambda (me target skill) (and
				(mp>reserve? me)
				(not (ref target 'bleeding?)) ; TODO check
				(member (ref target 'name) (list
					"Kaysha Herald Of Ikaros" ; Bleed.
					"Soul Scavenger" ; Sleep.
					"Princess Molrang" ; Poison.
				) string-ci=?)
				(>= (- (timestamp) (or (ref skill 'last-usage) 0)) 30) ; Delay 30s.
			)))

			(cons 'drain-health  (lambda (me target . rest) (and
				(hp-danger? me)
				(mp>shortage? me)
				(not (or (ref target 'boss?) (ref target 'minion?))) ; Ineffective for targets with high physical defense.
			)))

			(cons 'elemental-heal  (lambda (me . rest) (and
				(hp-danger? me)
				(mp>shortage? me)
			)))
			(cons 'bandage (lambda (me . rest) (and
				(ref me 'bleeding?)
			)))
			#|(cons 'cure-bleeding (lambda (me . rest) (and
				(ref me 'bleeding?)
			)))
			(cons 'poison-recovery (lambda (me . rest) (and
				(ref me 'poisoned?)
			)))|#

			(cons 'spoil (lambda (me target . rest) (and
				(npc? target)
				(member (ref me 'class) (list 'dwarven-fighter 'scavenger 'bounty-hunter) eq?)
				(not (or (ref target 'boss?) (ref target 'minion?)))
				(not (ref target 'spoiled?))
			)))

			(cons 'war-cry (lambda (me target . rest) (and
				(or (ref target 'boss?) (ref target 'minion?))
			)))
			(cons 'rage (lambda (me target . rest) (and
				(or (ref target 'boss?) (ref target 'minion?))
			)))
			(cons 'bear-spirit-totem (lambda (me target . rest) (and
				(or (ref target 'boss?) (ref target 'minion?))
			)))

			(cons 'ultimate-defense  (lambda (me . rest) (and
				(hp-critical? me)
			)))
			(cons 'ultimate-evasion  (lambda (me . rest) (and
				(<= (hp-ratio me) 1/4)
			)))
		)
	)
	(define (command-assist cn br author-id [skills (list)])
		(let* ((wr (connection-world cn)) (author (object-ref wr author-id)))
			(if author
				(let ((target-id (ref author 'target-id)) (me (world-me wr)))
					(when (and (fighter-type? me) target-id (not (in-party? (world-party wr) target-id)))
						(update-creature! me (list (cons 'casting #f))) ; TODO fix casting on timer.
						(brain-do! br (program program-slay target-id (alist-merge (skill-settings wr) (filter pair? skills)) (lambda (me target)
							(if (fighter-type? me) ; (or ... (eq? (ref me 'race) 'orc))
								(when (not (attacking? me)) (attack cn))
								(move-to cn (get-position author) 50)
							)
						)))
					)
				)
				(say cn "Don't see the requester.")
			)
		)
	)

	(define (parse-buffs me pack)
		(case pack
			(("ww") (const (list 'wind-walk)))
			(("solo") (lambda (character)
				(if (mystic-type? character)
					(list 'empower 'acumen 'berserker-spirit 'shield 'wind-walk)
					(list 'vampiric-rage 'might 'death-whisper 'focus 'shield 'guidance 'regeneration)
				)
			))
			(("phr") (lambda (character)
				(cond
					((protagonist? character) (list))
					((or (fighter-type? character) (eq? (ref character 'race) 'orc))
						(case (ref me 'class)
							((shillien-elder) (list 'vampiric-rage 'might 'death-whisper))
							(else (list 'focus 'shield 'wind-walk))
						)
					)
					((mystic-type? character)
						(case (ref me 'class)
							((shillien-elder) (list 'empower 'shield))
							(else (list 'acumen 'wind-walk))
						)
					)
					(else (list))
				)
			))
			(("rb") (lambda (character)
				(cond
					((protagonist? character) (list))
					((or (tank-class? character) (member (ref character 'name) (list "Ekon" "Evdem")))
						(case (ref me 'class)
							((shillien-elder) (list 'guidance 'vampiric-rage 'death-whisper 'might)) ; C-grade penalty.
							(else (list 'focus 'shield 'regeneration))
						)
					)
					((fighter-type? character)
						(case (ref me 'class)
							((shillien-elder) (list 'death-whisper 'might))
							(else (list 'focus))
						)
					)
					((support-class? character) (list 'acumen))
					((wizard-class? character) (list 'empower 'acumen))
					((and (eq? (ref character 'race) 'orc) (mystic-type? character)) (list 'acumen))
					; TODO Orc's buffs.
					(else (list))
				)
			))
			(("holy") (lambda (character)
				(if (or (fighter-type? character) (eq? (ref character 'race) 'orc))
					(list 'holy-weapon)
					(list)
				)
			))
			(("bers") (lambda (character)
				(list 'berserker-spirit)
			))
			(else (const (list)))
		)
	)
	(define (command-buff cn br author-id arguments)
		(let* ((wr (connection-world cn)) (me (world-me wr)))
			(when (and (support-class? me) (not (null? arguments)))
				(let ((author (object-ref wr author-id)))
					(if author
						(brain-do! br (program program-bless
							(parse-targets wr author (cdr arguments) "target")
							(parse-buffs me (car arguments))
						))
						(say cn "Don't see the requester.")
					)
				)
			)
		)
	)
)
