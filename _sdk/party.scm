#lang racket
(require
	(only-in srfi/1 fold)
	(only-in yaml read-yaml)
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
		"model/character.scm"
		"model/party.scm"
		"model/world.scm"
		"api/move_behind.scm"
		"api/gesture.scm"
		; "api/attack.scm"
		; "api/auto_shot.scm"
		"api/say.scm"
		"api/use_skill.scm"
		"api/party_crown.scm"
		"api/logout.scm"
		(only-in "program/program.scm" program program-id)
		"program/idle.scm"
		"program/partying.scm"
		"program/make_party.scm"
		"program/auto_confirm.scm"
		"program/follow_chase.scm"
		; "program/follow_repeat.scm"
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

(define (run cn wr me events party)
	(define br (make-brain cn program-idle))

	(brain-load! br
		(program program-partying party)
		(program program-auto-confirm)
	)

	(do ((ev (sync events) (sync events))) ((eq? (car ev) 'disconnect))
		; Triggers space.
		(case-event ev
			; Standard events.
			('creature-update (id changes)
				(when (eq? (get-class me) 'orc-shaman) (cond
					((car (or (ref changes 'poisoned?) (cons #f #f))) (say cn "Poisoned!"))
					((car (or (ref changes 'bleeding?) (cons #f #f))) (say cn "Ignited!"))
				))
			)
			('item-spawn (id . rest) ; Auto loot if I'm looter.
				(when (and (eq? looter (object-id me)) (not (eq? (program-id (brain-active br)) 'program-loot)))
					(brain-do! br (program program-loot #f) #t)
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

					(("assist") (command-assist cn br author-id (list
						(cons 'hate (lambda (me target skill) (and
							(eq? tank (object-id me))
							(>= (mp-ratio me) 1/10)
							(not (eq? (ref target 'target-id) (object-id me)))
							(>= (- (timestamp) (or (ref skill 'last-usage) 0)) 3) ; Delay 3s.
						)))
					)))
					(("tank") (let ((name (list-try-ref command 1 "?")))
						(cond
							((string-ci=? name "?") (let ((character (object-ref wr tank))) (when character
								(say cn (format "Tank: ~a." (ref character 'name)) 'chat-channel/party)
							)))
							((string-ci=? name "off") (set! tank #f))
							((string-ci=? name (ref me 'name))
								(set! tank (object-id me))
								(use-skill cn (car (select-skills 'majesty)))
							)
						)
					))
					(("looter") (let ((name (list-try-ref command 1 "?")))
						(cond
							((string-ci=? name "?") (let ((character (object-ref wr looter))) (when character
								(say cn (format "Looter: ~a." (ref character 'name)) 'chat-channel/party)
							)))
							((string-ci=? name "off") (set! looter #f))
							((string-ci=? name (ref me 'name))
								(set! looter (object-id me))
								(brain-do! br (program program-loot #f) #t)
							)
						)
					))
					(("buff")
						(command-buff cn br author-id (cdr command))
						(when (and (fighter-type? me) (member (ref me 'race) (list 'light-elf 'dark-elf) eq?)) ; For support level < 44.
							(use-skill cn (car (select-skills 'defense-aura)))
						)
					)
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

(define (instance host port account password name party)
	(apf-info "Running thread for ~a." (string-titlecase name))
	(let-values (((cn wr me events) (bootstrap host port account password name)))
		(apf-info "Player ~a has been logged in." (string-titlecase name))
		(let ((thev (wrap-evt (thread-receive-evt) (lambda args (make-event (thread-receive))))))
			(run cn wr me (choice-evt events thev) party)
		)
		(apf-info "Player ~a has been logged out." (string-titlecase name))
	)
)
(define (find-party cfg name)
	(let ((parties (ref cfg "party")))
		(let ((party (if parties (ref parties name) #f)))
			(or party (raise-user-error "Party is not found."))
		)
	)
)
(define (parse-shell [command-line (current-command-line-arguments)]) ; Syntax: racket realm.scm config.yaml party
	(cond
		((and (> (vector-length command-line) 1) (file-exists? (vector-ref command-line 0))) ; Login via config file.
			(let ((config (call-with-input-file (vector-ref command-line 0) read-yaml)) (party (vector-ref command-line 1)))
				(values (or (ref config "host") "localhost") (or (ref config "port") 2106) (find-party config party) (ref config "password"))
			)
		)
		(else (raise-user-error "Insufficient authentication data."))
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
(let-values (((host port party password) (parse-shell)))
	(apf-info "Spawning ~a threads." (- (length party) 1)) ; Skip leader.
	(let ((threads (map (lambda (name) (sleep 1/3) (thread (bind instance host port name password name party))) (cdr party))))
		(with-handlers ((exn:break? (lambda (e) (terminate threads) (wait threads))))
			(wait threads)
		)
		(apf-info "All threads was terminated.")
	)
)
