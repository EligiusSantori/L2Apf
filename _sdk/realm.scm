#lang racket
(require
	(only-in srfi/1 fold)
	(only-in yaml read-yaml)
	db/sqlite3
	(relative-in "../."
		"library/extension.scm"
		"library/geometry.scm"
		"library/date_time.scm"
		"system/structure.scm"
		; "system/connection.scm"
		"system/event.scm"
		"system/log.scm"
		"system/debug.scm"
		"model/skill.scm"
		"model/object.scm"
		"model/creature.scm"
		"model/npc.scm"
		"model/character.scm"
		"model/party.scm"
		"model/world.scm"
		"api/move_behind.scm"
		"api/gesture.scm"
		; "api/attack.scm"
		; "api/auto_shot.scm"
		"api/say.scm"
		"api/use_skill.scm"
		"api/toggle_skill.scm"
		"api/party_crown.scm"
		"api/return.scm"
		"api/logout.scm"
		(only-in "program/program.scm" program-id)
		"program/idle.scm"
		"program/partying.scm"
		"program/auto_confirm.scm"
		"program/follow_chase.scm"
		; "program/follow_repeat.scm"
		"program/report.scm"
		"program/loot.scm"
		"program/relax.scm"
		"program/bless.scm"
		"program/support.scm"
		; "program/auto_return.scm"
		; "program/escape.scm"
		; "program/travel.scm"
		"program/brain.scm"
		"bootstrap.scm"
	)
	"language.scm"
)

; Logic code.

(define tank #f)
(define looter #f)
(define navigator #f)
(define loot-radius 500)
(define wizard-delay 10) ; 10s


(define (align cn me pivot members arc [angle 0] [gap 50])
	(let ((my-id (object-id me)) (count (length members)))
		(if (and (member my-id members =) (> count 1))
			(let ((index (index-of (sort members <) my-id =)) (start (- angle (/ arc 2))))
				(move-behind cn pivot gap (+ start (* (/ arc (- count 1)) index)))
			)
			(move-behind cn pivot gap angle)
		)
	)
)
(define (command-affected wr channel author)
	(remove (object-id author) (case channel
		((chat-channel/all) (map object-id (near wr (get-position author) 1250 character?)))
		((chat-channel/shout chat-channel/trade) (map object-id (near wr (get-position author) 16384 character?)))
		((chat-channel/party) (party-members (world-party wr)))
		; TODO ((chat-channel/clan) ...)
		; TODO ((chat-channel/alliance) ...)
		((chat-channel/tell) (list (object-id (world-me wr))))
		(else (list))
	) =)
)
(define (aimed-at-me wr)
	(let ((my-id (object-id (world-me wr))))
		(fold-objects wr (list) (lambda (object r)
			(if (and (npc? object) (eq? (ref object 'target-id) my-id))
				(cons object r)
				r
			)
		))
	)
)
(define (skill-exists wr name) (find-skill wr name))
(define (weapon-type wr) (let ((item (get-weapon wr))) (and item (ref item 'item-type))))
(define (equip-bow? wr) (eq? (weapon-type wr) 'bow))
(define (medium-damage-skill-pointful? me target)
	(>= (or (ref target 'hp) 0) (* (ref me 'hp) 2/3)) ; Target.HP > (2/3)Me.HP
)
(define (assist cn wr br author-id)
	(let ((me (world-me wr)))
		(command-assist cn br author-id (list
			(cons 'aqua-swirl (lambda (me target skill) (and
				(< (ref me 'specialty) 2)
				(medium-damage-skill-pointful? me target)
				(>= (- (timestamp) (or (ref skill 'last-usage) 0)) wizard-delay)
			)))
			(cons 'blaze (lambda (me target skill) (and
				(< (ref me 'specialty) 2)
				(medium-damage-skill-pointful? me target)
				(>= (- (timestamp) (or (ref skill 'last-usage) 0)) wizard-delay)
			)))
			(cons 'twister (lambda (me target skill) (and
				(< (ref me 'specialty) 2)
				(medium-damage-skill-pointful? me target)
				(>= (- (timestamp) (or (ref skill 'last-usage) 0)) wizard-delay)
			)))
			(cons 'hurricane (lambda (me target skill) (and
				(medium-damage-skill-pointful? me target)
				(>= (- (timestamp) (or (ref skill 'last-usage) 0)) wizard-delay)
			)))

			(cons 'hate (lambda (me target skill) (and
				(eq? tank (object-id me))
				(not (eq? (ref target 'target-id) (object-id me)))
				(>= (- (timestamp) (or (ref skill 'last-usage) 0)) 5) ; Delay 5s.
			)))
			(cons 'charm (lambda (me target skill) (let ((victim (get-target wr target))) (and
				victim
				(and tank (not (eq? tank (object-id victim))))
				(<= (hp-ratio victim) 1/2)
			))))
			(cons 'confusion (lambda (me target skill) (let ((victim (get-target wr target))) (and
				(character? victim)
				(or (not tank) (not (eq? tank (object-id victim))))
				(<= (hp-ratio victim) 1/2)
			))))
			(and (not (eq? looter (object-id me)))
				(cons 'spoil (const #f))
			)
			(and (member (ref me 'name) (list "Evdem") string-ci=?)
				(cons 'power-strike (const #f))
			)
			(and (member (ref me 'name) (list "Ekon") string-ci=?)
				(cons 'sting (const #f))
			)
		))
	)
)

(define (run cn wr me events party config)
	(define br (make-brain cn (make-program-idle)))

	(brain-load! br
		(make-program-partying party)
		(make-program-auto-confirm)
		(make-program-report)
	)

	(do ((ev (sync events) (sync events))) ((eq? (car ev) 'disconnect))
		; Triggers space.
		(case-event ev
			; Standard events.
			('creature-update (id changes)
				(when (= id (object-id me))
					(cond ; Auto-cure.
						((and (ref me 'poisoned?) (or (<= (hp-ratio me) 1/5) (and (ref changes 'poisoned?) (<= (hp-ratio me) 2/3))))
							(command-use cn (list "antidote"))
						)
						((and (ref me 'bleeding?) (or (<= (hp-ratio me) 1/5) (and (ref changes 'bleeding?) (<= (hp-ratio me) 2/3))))
							(command-use cn (list "bandage"))
						)
					)
				)
			)
			('change-target (subject-id target-id previous-id) ; Escape under attack if I'm mage.
				(let ((creature (object-ref wr subject-id)))
					(when (and navigator (npc? creature) (or (mystic-type? me) (equip-bow? wr)))
						(cond
							((eq? target-id (object-id me))
								(brain-do! br (make-program-follow-chase navigator 50) #t)
							)
							((and (eq? previous-id (object-id me)) (eq? (program-id (brain-active br)) 'program-follow-chase))
								(brain-stop! br (brain-active br))
							)
						)
					)
				)
			)
			('item-spawn (id . rest) ; Auto loot if I'm looter.
				(when (and (eq? looter (object-id me)) (not (eq? (program-id (brain-active br)) 'program-loot)))
					(brain-do! br (make-program-loot loot-radius))
				)
			)
			('die (subject-id . rest) ; Auto sweep.
				(when (and (eq? looter (object-id me)) (not (eq? (program-id (brain-active br)) 'program-loot)))
					(let ((creature (object-ref wr subject-id)))
						(when (and (npc? creature) (ref creature 'spoiled?) (find-skill wr 'sweeper))
							(brain-do! br (make-program-loot loot-radius))
						)
					)
				)
			)
			('message (author-id channel name text) ; Execute commands from game chat.
				(let ((command (parse-command wr text channel author-id))) (when command (case (car command)
					(("hello") (gesture cn 'gesture/hello))
					(("show") (command-show cn (cdr command)))

					(("pick") (command-pick cn))
					(("use") (command-use cn (cdr command)))
					(("drop") (command-drop cn br (cdr command)))
					(("skill") (command-skill cn (cdr command)))
					(("rech") (when (skill-exists wr 'recharge) (command-repeat cn br 'recharge author-id (cdr command))))
					(("heal") (when (skill-exists wr 'heal) (command-repeat cn br 'heal author-id (cdr command))))
					(("power") (let ((is? (not (string=? (list-try-ref command 1 "on") "off"))))
						(when (and (not (equip-bow? wr)) (not (eq? (ref me 'race) 'orc)))
							(toggle-skill cn (find-skill wr 'vicious-stance) is?)
						)
						(toggle-skill cn (find-skill wr 'soul-cry) is?)
						; (auto-shot cn is? (if (fighter-type? me) 'soulshot 'blessed-spiritshot) (weapon-grade me))
					))
					(("prep")
						(use-skill cn (find-skill wr 'majesty))
						(use-skill cn (find-skill wr 'battle-roar))
					)
					(("delay") (let ((interval (or (string->number (list-try-ref command 1 "7")) 7))) (set! wizard-delay interval)))

					(("give") (command-give cn br name (cdr command)))

					(("follow") (let ((gap (or (string->number (list-try-ref command 1 "30")) 30))) ; TODO comb style
						(set! navigator author-id)
						(brain-clear! br)
						(brain-do! br (make-program-follow-chase author-id gap))
					))
					(("return") (let ((author (object-ref wr author-id)) (gap (or (string->number (list-try-ref command 1 "50")) 50)))
						(when (and author (get-position author))
							(brain-clear! br)
							(let* ((target (or (get-target wr author) author))
									(angle (if (object=? author target) (get-angle wr author) (creatures-angle author target)))
									(members (command-affected wr channel author)))
								(cond
									((null? members) (align cn me target (list) 0 (+ angle pi) gap)) ; Common.
									((not tank) (align cn me target members (- 2pi (/ 2pi (length members))) 0 gap)) ; For party.
									((= (object-id me) tank) (align cn me target (list tank) (/ pi 10) angle gap)) ; For tank.
									(else (align cn me target (remove tank members =) pi/2 (+ angle pi) gap)) ; For others.
								)
							)
						)
					))
					(("town") (return cn))

					(("raid")
						(set! navigator author-id)
						(brain-clear! br)
						(cond
							((support-class? me)
								; (brain-do! br (make-program-follow-chase author-id 100))
								(if (member (ref me 'name) (list "kae" "oakia" "vesta") string-ci=?)
									(brain-do! br (make-program-support 2/3 1/5 1/2 1/5 #f #t))
									(brain-do! br (make-program-support 2/3 1/5 #f #f #f #t))
								)
							)
							(else (assist cn wr br author-id))
						)
					)
					(("assist")
						(set! navigator author-id)
						(assist cn wr br author-id)
					)
					(("support") (when (support-class? me)
						(brain-do! br (make-program-support 2/3 1/5 1/2 1/5 #f #f))
					))
					(("tank") (let ((name (list-try-ref command 1 "?")))
						(cond
							((string-ci=? name "?") (let ((character (object-ref wr tank))) (when character
								(say cn (format "Tank: ~a." (ref character 'name)))
							)))
							((string-ci=? name "off") (set! tank #f))
							((string-ci=? name (ref me 'name)) (set! tank (object-id me)))
						)
					))
					(("looter") (let ((name (list-try-ref command 1 "?")))
						(cond
							((string-ci=? name "?") (let ((character (object-ref wr looter))) (when character
								(say cn (format "Looter: ~a." (ref character 'name)))
							)))
							((string-ci=? name "off") (set! looter #f))
							((string-ci=? name (ref me 'name))
								(set! looter (object-id me))
								(brain-do! br (make-program-loot loot-radius) #t)
							)
						)
					))
					(("buff") (command-buff cn br author-id (cdr command) config))
					(("relax") (let ((duration (or (string->number (list-try-ref command 1 "0")) 0)))
						; (when (or (> duration 0) (not (eq? (program-id (brain-active br)) 'program-relax)))
							(brain-clear! br)
							(brain-do! br (make-program-relax duration))
						; )
					))

					(("clear") (brain-clear! br))
					(("bye") (logout cn))
				)))
			)
			('break () (logout cn))

			; Custom events.
		)

		; Programs space.
		(brain-run! br ev)
	)
)

; System code.

(define db (sqlite3-connect #:database "apf.db" #:mode 'read-only))
(define (instance host port account password name party config)
	(apf-info "Running thread for ~a." (string-titlecase name))
	(let-values (((cn wr me events) (bootstrap host port account password name db)))
		(apf-info "Player ~a has been logged in." (string-titlecase name))
		(let ((thev (wrap-evt (thread-receive-evt) (lambda args (make-event (thread-receive))))))
			(run cn wr me (choice-evt events thev) party config)
		)
		(apf-info "Player ~a has been logged out." (string-titlecase name))
	)
)
(define (parse-shell [command-line (current-command-line-arguments)]) ; Syntax: racket realm.scm config.yaml party
	(cond
		((and (> (vector-length command-line) 1) (file-exists? (vector-ref command-line 0))) ; Login via config file.
			(let ((config (call-with-input-file (vector-ref command-line 0) read-yaml)))
				(values
					(or (ref config "host") "localhost")
					(or (ref config "port") 2106)
					(ref config "password")
					(cdr (vector->list command-line))
					config
				)
			)
		)
		(else (raise-user-error "Insufficient authentication data."))
	)
)
(define (find-party cfg name)
	(let ((party (ref cfg "party" name)))
		(or party (raise-user-error "Party is not found."))
	)
)
(define (parse-bunch bunch config)
	(if (string-starts? bunch "party.")
		(find-party config (cadr (string-split bunch "." #:repeat? #f)))
		(list bunch)
	)
)
(define (prepare)
	(let-values (((host port password bunches config) (parse-shell)))
		(apply append (fold (lambda (bunch r)
			(let* ((names (parse-bunch bunch config)) (party (if (> (length names) 1) names #f)))
				(cons (map (lambda (name) (bind instance host port name password name party config)) names) r)
			)
		) (list) bunches))
	)
)
(define (terminate threads)
	(map (lambda (th)
		(when (thread-running? th)
			(thread-send th 'break #f)
		)
	) threads)
)
(define (wait threads)
	(define (sync-many events) (and (not (null? events)) (apply sync/enable-break events)))
	(let ((events (map thread-dead-evt (filter (compose not thread-dead?) threads))))
		(do ((ev (sync-many events) (sync-many events))) ((not ev))
			(set! events (remove ev events eq?))
		)
	)
)

(global-port-print-handler apf-print-handler)
(let ((todo (prepare)))
	(apf-info "Spawning ~a threads." (length todo))
	(let ((threads (map (lambda (fn) (sleep 1/3) (thread fn)) todo)))
		(with-handlers ((exn:break? (lambda (e) (terminate threads) (wait threads))))
			(wait threads)
		)
	)
	(apf-info "All threads was terminated.")
)
