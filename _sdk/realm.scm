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
		(only-in "program/program.scm" program program-id)
		"program/idle.scm"
		"program/partying.scm"
		"program/make_party.scm"
		"program/auto_confirm.scm"
		"program/follow_chase.scm"
		; "program/follow_repeat.scm"
		"program/report.scm"
		"program/loot.scm"
		"program/relax.scm"
		; "program/support.scm"
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
(define loot-radius 500)

(define (party-angle wr [arc 2pi] [except-id #f])
	(let ((members (remove except-id (party-members (world-party wr)) eq?)))
		(if (not (null? members))
			(let ((gap (/ arc (- (length members) 1))) (my-id (object-id (world-me wr))))
				(cdr (fold (lambda (member-id r)
					(cons
						(+ (car r) gap)
						(if (= my-id member-id) (car r) (cdr r))
					)
				) (cons 0 #f) (sort (cdr members) <)))
			)
			#f
		)
	)
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

(define (run cn wr me events party)
	(define br (make-brain cn program-idle))

	(brain-load! br
		(program program-partying party)
		(program program-auto-confirm)
		(program program-report)
	)

	(do ((ev (sync events) (sync events))) ((eq? (car ev) 'disconnect))
		; Triggers space.
		(case-event ev
			; Standard events.
			('creature-update (id changes)
				(when (= id (object-id me))
					(cond ; Auto-cure.
						((and (ref me 'poisoned?) (or (<= (hp-ratio me) 1/3) (and (ref changes 'poisoned?) (<= (hp-ratio me) 2/3))))
							(command-use cn (list "antidote"))
						)
						((and (ref me 'bleeding?) (or (<= (hp-ratio me) 1/3) (and (ref changes 'bleeding?) (<= (hp-ratio me) 2/3))))
							(command-use cn (list "bandage"))
						)
					)
				)
			)
			('change-target (subject-id target-id previous-id) ; Escape under attack if I'm mage.
				(let ((creature (object-ref wr subject-id)))
					(when (and (npc? creature) (mystic-type? me) (not (eq? (ref me 'race) 'orc)) (world-party wr))
						(cond
							((eq? target-id (object-id me))
								(brain-do! br (program program-follow-chase (party-leader (world-party wr)) 50) #t)
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
					(brain-do! br (program program-loot (find-skill wr 'sweeper) loot-radius))
				)
			)
			('die (subject-id . rest) ; Auto sweep.
				(when (and (eq? looter (object-id me)) (not (eq? (program-id (brain-active br)) 'program-loot)))
					(let ((creature (object-ref wr subject-id)))
						(when (and (npc? creature) (ref creature 'spoiled?) (find-skill wr 'sweeper))
							(brain-do! br (program program-loot #t loot-radius))
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
					(("rech") (command-repeat cn br 'recharge author-id (cdr command)))
					(("heal") (command-repeat cn br 'heal author-id (cdr command)))
					(("power") (let ((is? (not (string=? (list-try-ref command 1 "on") "off"))))
						(toggle-skill cn (find-skill wr 'vicious-stance) is?)
						; (toggle-skill cn (find-skill wr 'soul-cry) is?)
						; (auto-shot cn is? (if (fighter-type? me) 'soulshot 'blessed-spiritshot) (weapon-grade me))
					))
					(("prep")
						(use-skill cn (find-skill wr 'majesty))
						(use-skill cn (find-skill wr 'battle-roar))
					)

					(("invite") (brain-do! br (program program-make-party party 'finder 0.5)))
					(("crown") (party-crown cn name))

					(("follow") (let ((gap (or (string->number (list-try-ref command 1 "30")) 30))) ; TODO comb style
						(brain-clear! br)
						(brain-do! br (program program-follow-chase author-id gap))
					))
					(("return") (let ((author (object-ref wr author-id))) (when (and author (get-position author))
						(brain-clear! br)
						(let* ((target (or (get-target wr author) author))
								(angle (if (object=? author target) (get-angle wr author) (creatures-angle author target))))
							(cond
								((not tank) (move-behind cn target 50 (party-angle wr)))
								((= (object-id me) tank) (move-behind cn target 50 angle))
								(else (move-behind cn target 50 (+ (party-angle wr pi/2 tank) angle (- pi/4) pi)))
							)
						)
					)))
					(("town") (return cn))

					(("assist") (command-assist cn br author-id (list
						(cons 'hate (lambda (me target skill) (and
							(eq? tank (object-id me))
							(>= (mp-ratio me) 1/10)
							(not (eq? (ref target 'target-id) (object-id me)))
							(>= (- (timestamp) (or (ref skill 'last-usage) 0)) 5) ; Delay 5s.
						)))
						(cons 'charm (lambda (me target skill) (let ((victim (get-target wr target))) (and
							victim
							(and tank (not (eq? tank (object-id victim))))
							(>= (mp-ratio me) 1/10)
							(<= (hp-ratio victim) 1/2)
						))))
						(cons 'confusion (lambda (me target skill) (let ((victim (get-target wr target))) (and
							victim
							(or (not tank) (not (eq? tank (object-id victim))))
							(>= (mp-ratio me) 1/10)
							(<= (hp-ratio victim) 1/2)
						))))
						; (cons 'sting (const #f)) ; TODO if not tank
					)))
					#|(("support") (when (support-class? me)
						(brain-do! br (program program-support
							(lambda (character)
								(and
									(<= (hp-ratio character) 2/3)
									(> (mp-ratio me) 1/10)
								)
							)
							(lambda (character)
							 	(and
									(skill-ref wr (find-skill wr 'recharge))
									(mystic-type? character)
									(<= (mp-ratio character) 19/20)
									(> (mp-ratio me) 1/10)
								)
							) #f)
						)
					))|#
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
								(brain-do! br (program program-loot #f) #t)
							)
						)
					))
					(("buff") (command-buff cn br author-id (cdr command)))
					(("relax") (let ((duration (or (string->number (list-try-ref command 1 "0")) 0)))
						(brain-clear! br)
						(brain-do! br (program program-relax duration))
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
(define (instance host port account password name party)
	(apf-info "Running thread for ~a." (string-titlecase name))
	(let-values (((cn wr me events) (bootstrap host port account password name db)))
		(apf-info "Player ~a has been logged in." (string-titlecase name))
		(let ((thev (wrap-evt (thread-receive-evt) (lambda args (make-event (thread-receive))))))
			(run cn wr me (choice-evt events thev) party)
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
	(let ((parties (ref cfg "party")))
		(let ((party (if parties (ref parties name) #f)))
			(or party (raise-user-error "Party is not found."))
		)
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
			(let* ((names (parse-bunch bunch config)) (party (if (null? (cdr names)) #f names)) (rest (if party (cdr party) names))) ; Skip leader.
				(cons (map (lambda (name) (bind instance host port name password name party)) rest) r)
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
