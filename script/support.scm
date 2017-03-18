#lang racket
(require
	srfi/1
	"_misc.scm"
	(relative-in "../.."
		"library/extension.scm"
		"library/structure.scm"
		"library/geometry.scm"
		"system/contract.scm"
		"logic/object.scm"
		"logic/creature.scm"
		"logic/npc.scm"
		"logic/character.scm"
		"logic/antagonist.scm"
		"logic/protagonist.scm"
		"logic/skill.scm"
		"logic/world.scm"
		"api/connect.scm"
		"api/login.scm"
		"api/select_server.scm"
		"api/select_character.scm"
		"api/use_skill.scm"
		"api/gesture.scm"
		"api/move_to.scm"
		"api/move_on.scm"
		"api/pick_up.scm"
		"api/target.scm"
		"api/cancel.scm"
		"api/action.scm"
		"api/attack.scm"
		"api/reply.scm"
		"api/say.scm"
		"api/run.scm"
		"api/sit.scm"
		"api/logout.scm"
	)
)

(define skill-id/vampiric-rage 1268)
(define skill-id/focus 1077)
(define skill-id/might 1068)
(define skill-id/shield 1040)
(define skill-id/empower 1059)
(define skill-id/concentration 1078)
(define skill-id/wind-walk 1204)
(define skill-id/mental-shield 1035)
(define skill-id/heal 1011)
(define skill-id/battle-heal 1015)
(define skill-id/recharge 1013)
(define skill-id/cure-poision 1012)
(define skill-id/resurrection 1016)

(define states (list
	'state/buffing
	;'state/healing
	;'state/recharging
	;'state/caring
	;'state/partying
	'state/following
	'state/traveling
	;'state/escaping
	'state/resting
	'state/nothing
))

(define (is-fighter? creature)
	(displayln (@: creature 'class-id))
	(member (@: creature 'class-id) (list 
		0 1 2 88 3 89 4 5 90 6 91 7 8 93 9 92 ; Human
		18 19 20 99 21 100 22 23 101 24 102 ; Dark Elf
		31 32 33 106 34 107 35 36 108 37 109 ; Elf
		44 45 46 113 47 48 114 ; Orc
		53 54 55 117 56 57 118 ; Dwarf
	))
)

(define (is-mage? creature)
	(displayln (@: creature 'class-id))
	(member (@: creature 'class-id) (list 
		10 11 12 94 13 95 14 96 15 16 97 17 98 ; Human
		25 26 27 103 28 104 29 30 105 ; Dark Elf
		38 39 40 110 41 111 42 43 112 ; Elf
		49 50 51 115 52 116  ; Orc
	))
)

(define (get-buff-list creature) ; TODO filter through current skill table
	(cond
		((is-fighter? creature) (list ; TODO extend
				skill-id/vampiric-rage
				skill-id/focus
				skill-id/might
				skill-id/shield
				skill-id/mental-shield
				skill-id/wind-walk
		))
		((is-mage? creature) (list ; TODO extend
				skill-id/empower
				skill-id/concentration
				skill-id/shield
				skill-id/mental-shield
				skill-id/wind-walk
		))
		(else (list))
	)
)


#|(define (hp-danger? me)
	(let ((hp (@: me 'hp)) (max-hp (@: me 'max-hp)))
		(<= (/ hp max-hp) 1/3)
	)
)
(define (hp-critical? me)
	(let ((hp (@: me 'hp)) (max-hp (@: me 'max-hp)))
		(<= (/ hp max-hp) 1/10)
	)
)
(define (mp-economy? me)
	(let ((mp (@: me 'mp)) (max-mp (@: me 'max-mp)))
		(<= (/ mp max-mp) (cond
			((artisan? me) 1/3)
			((scavenger? me) 2/3)
			(else 1)
		))
	)
)|#

; TODO follow
; TODO heal
; TODO recharge

(let-values (((account password host port name) (parse-protocol (current-command-line-arguments))))
	(let ((connection (connect host port)))
		(let ((world (first (login connection account password))))
			(let ((me (@: (select-server connection world) name)))
				(let ((events (select-character connection me)))			
					(define state 'state/nothing)
					(define buff-queue (list))
					;(define response-to)
					
					(define target/sync (make-contract target (lambda (event)
						(and
							(equal? (first event) 'change-target)
							(equal? (second event) (@: me 'object-id))
						)
					)))
					
					(define cancel/sync (make-contract cancel (lambda (event)
						(and
							(equal? (first event) 'change-target)
							(equal? (second event) (@: me 'object-id))
						)
					)))
					
					(define (buff-iterate)
						(if (not (null? buff-queue))
							(let ((queue buff-queue))
								(set! buff-queue (cdr queue))
								(use-skill connection (car queue))
							)
							(set! state 'state/nothing)
						)
					)
					
					(define (buff master-id)
						(let ((master (object-ref world master-id)))
							(if master
								(let ((queue (get-buff-list master)))
									(when (not (null? queue))
										(set! state 'state/buffing)
										(set! buff-queue queue)
										(target/sync connection master-id)
										(buff-iterate)
									)
								)
								(say connection "I don't see you")
							)
						)
					)
					
					;(define (follow-iterate)
					;
					;)
					
					(define (follow master-id)
						(let ((master (object-ref world master-id)))
							(if master
								(begin
									(set! state 'state/following)
									(target connection master-id)
									(when (@: me 'sitting?) (sit connection #f) (sleep 1/3))
									(move-to connection (or (@: master 'destination) (@: master 'position)))
								)
								(say connection "I don't see you")
							)
						)
					)
					
					(define (return master-id)
						(set! state 'state/nothing)
						(when (@: me 'target-id) (cancel connection))
						(when (@: me 'sitting?) (sit connection #f) (sleep 1/3))
						(let ((to (object-ref world master-id)))
							(when to (move-to connection (@: to 'position)))
						)
					)
					
					(define traveller (make-traveller connection))
					
					(define (relax)
						(let ((sitting? (@: me 'sitting?)) (hp (@: me 'hp)) (max-hp (@: me 'max-hp)) (mp (@: me 'mp)) (max-mp (@: me 'max-mp)))
							(set! state 'state/resting)
							(when (@: me 'target-id)
								(cancel connection)
								(sleep 1/3)
							)
							; TODO stay if running
							(when (and (not sitting?) (or (< hp max-hp) (< mp max-mp)))
								(sit connection #t)
							)
						)
					)
					
					(let loop ()
						(let ((event (sync events)))
							(case (if event (car event) #f)
								; custom events
								
								; standard events
								((skill-launched) (let-values (((object-id skill-id level) (apply values (cdr event))))
									(when (and (equal? state 'state/buffing) (equal? object-id (@: me 'object-id))) ; dequeue and process next buff
										(buff-iterate)
									)
								))
								((change-moving) (let-values (((object-id position destination) (apply values (cdr event))))
									(when (and (equal? state 'state/following) (equal? object-id (@: me 'target-id))) ; follow if master
										(move-to connection (or destination position))
									)
									(when (and (equal? state 'state/traveling) (equal? object-id (@: me 'object-id))) ; travel next if it is me
										(when (zero? (traveller 'next))
											(logout connection) ;(set! state 'state/nothing)
										)
									)
								))
								#|((die) (let-values (((object-id return) (apply values (cdr event))))
									(when (equal? object-id (@: me 'object-id))
										(say connection "I have died" response-to)
									)
								))|#
								((ask) (let-values (((question data) (apply values (cdr event))))
									(reply connection question (equal? question 'ask/join-party))
								))
								((message) (let-values (((object-id channel author text) (apply values (cdr event))))
									(let ((command (parse-command text)))
										(if command
											(case (car command)
												(("hello") (gesture connection 'gesture/hello))
												(("bye") (logout connection))
												(("buff") (buff (case (if (not (null? (cdr command))) (second command) #f)
													(("self") (@: me 'object-id))
													; TODO my
													; TODO party (list (cons target-id skill-id) ...)
													(else object-id) ; me
												)))
												(("relax") (relax))
												(("travel") (if (null? (cdr command))
													(say connection (format "~a points left" (traveller 'left)) author)
													(let ((count (traveller (cond
															((= (length command) 4) (apply point/3d (map string->number (cdr command))))
															((= (length command) 2) (second command))
														))))
														(when (> count 0)
															(set! state 'state/traveling)
															(say connection (format "Traveling through ~a points" count) author)
														)
													)
												))
												; TODO (("travel") point / route [loop?] [speed_ratio] ) ; TODO feedback if died, feedback & logout when finished
												; TODO use
												; TODO say
												; TODO skill
												; TODO (("return")) ; TODO town, clanhall, ...
												; TODO route test / route finish
												(("follow") (follow object-id)) ; TODO normal (by timer), fast (to destination), accurately (full path)
												(("return") (return object-id))
												(else (say connection "I don't understand"))
											)
											(displayln (format-chat-message object-id channel author text))
										)
									)
								))
								((logout)
									(exit)
								)
							)
						)
						(loop)
					)
				)
			)
		)
	)
)