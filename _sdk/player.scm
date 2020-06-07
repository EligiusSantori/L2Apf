#lang racket
(require
	(only-in srfi/1 first second third fourth fifth sixth)
	srfi/13
	(only-in yaml read-yaml)
	(relative-in "../."
		"library/extension.scm"
		"system/structure.scm"
		; "system/connection.scm"
		"system/event.scm"
		"system/log.scm"
		"system/debug.scm"
		"model/object.scm"
		"model/creature.scm"
		"model/character.scm"
		"model/world.scm"
		"api/gesture.scm"
		"api/move_behind.scm"
		"api/say.scm"
		"api/logout.scm"
		(only-in "program/program.scm" program program-id)
		"program/idle.scm"
		"program/print.scm"
		; "program/radar.scm"
		"program/partying.scm"
		"program/auto_confirm.scm"
		"program/follow_chase.scm"
		"program/follow_repeat.scm"
		; "program/loot.scm"
		"program/relax.scm"
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

(define (run cn wr me events)
	(define br (make-brain cn program-idle))
	(brain-load! br
		(program program-print)
		(program program-partying)
		(program program-auto-confirm)
		; (program program-radar 'radar creature? (* 100 (or (ref me 'collision-radius) 10)))
	)

	; (interval! #:id 'to-do cn 3)

	(do ((event (sync events) (sync events))) ((eq? (car event) 'disconnect))
		; Triggers space.
		(case-event event
			; Standard events.
			('creature-create (id) ; Unhide builder character on login.
				(when (and (= (object-id me) id) (> (ref me 'access-level) 0))
					(say cn "hide off" 'chat-channel/game-master)
				)
			)
			#|('skill-launched (subject-id . rest)
				(when (and (= subject-id (object-id me)) (ref me 'in-combat?))
					(let ((target (get-target wr me)))
						(when (and (npc? target) (not (ref target 'alike-dead?)))
							(attack cn)
						)
					)
				)
			)
			('item-spawn (id . rest) ; Auto loot if scavenger or dwarven fighter.
				(when (not (eq? (program-id (brain-active br)) 'program-loot)) ;
					; (when (dwarf-or-scavenger? me) (brain-do! br (program program-loot #t 500))) ; For Grele.
					(brain-do! br (program program-loot #f 500) #t) ; For Gracia.
				)
			)
			('die (subject-id . rest) ; Auto loot if scavenger or dwarven fighter.
				(when (not (eq? (program-id (brain-active br)) 'program-loot)) ;
					(let ((creature (object-ref wr subject-id)))
						(when (and (npc? creature) (ref creature 'spoiled?) (dwarf-or-scavenger? me))
							(brain-do! br (program program-loot #t 500))
						)
					)
				)
			)|#
			('message (author-id channel author text) ; Execute commands from game chat.
				(let ((command (parse-command wr text channel author-id))) (when command (case (car command)
					(("hello") (gesture cn 'gesture/hello))
					(("show") (command-show cn (cdr command)))

					(("return") (let ((author (object-ref wr author-id))) (when author (move-behind cn author 50))))
					(("follow") (let ((gap (or (string->number (list-try-ref command 1 "30")) 30)))
						(brain-do! br (program program-follow-chase author-id gap))
					))
					(("follow+") (brain-do! br (program program-follow-repeat author-id 50)))

					(("assist") (command-assist cn br author-id))
					(("buff") (command-buff cn br author-id (cdr command)))
					; (("support") (when (support-class? me)
					; 	(brain-load! br (program program-follow-chase author-id 150))
					; 	(brain-do! br (program program-support #t ; For Gracia.
					; 		(lambda (character)
					; 		 	(and
					; 				(mystic-type? character)
					; 				(<= (mp-ratio character) 2/3)
					; 				(> (mp-ratio me) 1/5)
					; 			)
					; 		) #f)
					; 	)
					; ))
					(("relax") (let ((duration (or (string->number (list-try-ref command 1 "0")) 0)))
						(brain-do! br (program program-relax duration))
					))

					(("clear") (brain-clear! br))
					(("bye") (logout cn))
				)))
			)

			; Custom events.
			; ((radar) (let-values (((ids in out) (list->values (cdr event))))
			; 	(printf "~a ~a~n" in out)
			; ))
			('to-do ()
				; (printf "~a: My attackers: ~a~n" (format-time) (attackers me))
				; (let ((op (ref me 'position)) (cp (get-position me)) (d (ref me 'destination)))
				; 	(when d (printf "{move-progress ~a}~n" (exact-round (* (/ (points-distance op cp) (points-distance op d)) 100))))
				; 	(flush-output)
				; )
				; (map (lambda (object)
				; 	(printf "{Angle of ~v = ~a}~n" object (ref object 'angle))
				; ) (objects wr protagonist?))

				(void)
			)
		)

		; Programs space.
		(brain-run! br event)
	)
)

; System code.

#|(define (parse-config [command-line (current-command-line-arguments)])
	(fold (lambda (arg cfg)
		(if (and (not cfg) (string-ends? (string-downcase arg) ".yaml") (file-exists? arg))
			(call-with-input-file arg read-yaml)
			cfg
		)
	) #f (vector->list command-line))
)|#
(define (parse-uri uri)
	(let ((t (regexp-match #rx"^l2apf://([0-9A-Za-z_]+):([0-9A-Za-z]+)@([0-9A-Za-z\\.\\-]+):?([0-9]*)/([0-9A-Za-z]+)[\\?#/]?" uri)))
		(if t
			(list
				(fourth t) ; host
				(let ((port (fifth t))) ; port
					(if (string-null? port) 2106 (string->number port))
				)
				(second t) ; login
				(third t) ; password
				(sixth t) ; name
			)
			#f
		)
	)
)
(define (parse-shell [command-line (current-command-line-arguments)])
	(cond
		((and (> (vector-length command-line) 1) (file-exists? (vector-ref command-line 0))) ; Login via config file.
			(let ((config (call-with-input-file (vector-ref command-line 0) read-yaml)) (name (vector-ref command-line 1)))
				(values (or (ref config "host") "localhost") (or (ref config "port") 2106) name (ref config "password") name)
			)
		)
		((and (> (vector-length command-line) 0) (string-starts? (string-downcase (vector-ref command-line 0)) "l2apf:")) ; Login via URI.
			(let ((uri (parse-uri (vector-ref command-line 0))))
				(if uri
					(apply values uri)
					(raise-user-error "Authentication failed because URI is broken.")
				)
			)
		)
		(else (raise-user-error "Insufficient authentication data."))
	)
)
(define (terminate cn events)
	(logout cn)
	(let loop ()
		(case-event (sync events)
			('logout () (apf-info "Logged out."))
			(else (loop))
		)
	)
)

(global-port-print-handler apf-print-handler)
(let-values (((cn wr me events) (call/wv parse-shell bootstrap)))
	(apf-info "Player ~a has been logged in." (ref me 'name))
	(with-handlers ((exn:break? (lambda (e) (terminate cn events))))
		(run cn wr me events)
	)
)
