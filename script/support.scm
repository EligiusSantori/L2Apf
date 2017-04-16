#lang racket
(require
	srfi/1
	"_misc.scm"
	(relative-in "../.."
		"library/extension.scm"
		"library/structure.scm"
		"library/geometry.scm"
		"system/contract.scm"
		"model/object.scm"
		"model/creature.scm"
		"model/npc.scm"
		"model/character.scm"
		"model/antagonist.scm"
		"model/protagonist.scm"
		"model/skill.scm"
		"model/world.scm"
		"api/connect.scm"
		"api/login.scm"
		"api/select_server.scm"
		"api/select_character.scm"
		"api/use_skill.scm"
		"api/use_item.scm"
		"api/auto_shot.scm"
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
		"api/target_sync.scm"
		"api/cancel_sync.scm"
		"behavior/travelling.scm"
		"behavior/following.scm"
		"behavior/buffing.scm"
	)
)

(define skill-id/vampiric-rage 1268)
(define skill-id/focus 1077)
(define skill-id/death-whisper 1242)
(define skill-id/might 1068)
(define skill-id/shield 1040)
(define skill-id/guidance 1240)
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
	'state/supporting
	'state/buffing
	'state/following
	'state/traveling
	;'state/escaping
	'state/resting
	'state/nothing
))

(define (hp-danger? creature)
	(let ((hp (@: creature 'hp)) (max-hp (@: creature 'max-hp)))
		(displayln (list "hp-danger? " hp max-hp))
		(and hp max-hp
			(<= (/ hp max-hp) 1/3)
		)
	)
)
(define (hp-almost-full? creature)
	(let ((hp (@: creature 'hp)) (max-hp (@: creature 'max-hp)))
		(displayln (list "hp-almost-full? " hp max-hp))
		(and hp max-hp
			(>= (/ hp max-hp) 4/5)
		)
	)
)
(define (hp-critical? creature)
	(let ((hp (@: creature 'hp)) (max-hp (@: creature 'max-hp)))
		(displayln (list "hp-critical? " hp max-hp creature))
		(and hp max-hp
			(<= (/ hp max-hp) 1/10)
		)
	)
)
(define (mp-almost-full? creature)
	(let ((mp (@: creature 'mp)) (max-mp (@: creature 'max-mp)))
		(displayln (list "mp-almost-full? " mp max-mp))
		(and mp max-mp
			(>= (/ mp max-mp) 4/5)
		)
	)
)

(let-values (((account password host port name) (parse-protocol (current-command-line-arguments))))
	(let ((connection (connect host port)))
		(let ((world (findf (lambda (server) (@: server 'state)) (login connection account password))))
			(let ((me (cdr (assoc name (select-server connection world) string-ci=?))))
				(let ((events (select-character connection me)))			
					(define state 'state/nothing)
					;(define response-to)
					
					(define (get-buff-list object-id)
						(let ((creature (object-ref world object-id)))
							(if creature
								(cond
									((fighter? creature) (list ; TODO extend
											skill-id/wind-walk
											skill-id/vampiric-rage
											skill-id/death-whisper
											skill-id/focus
											skill-id/guidance
											skill-id/might
											skill-id/shield
											; skill-id/mental-shield ; buggy, temporary disabled
									))
									((mystic? creature) (list ; TODO extend
											skill-id/wind-walk
											skill-id/empower
											skill-id/concentration
											skill-id/shield
											; skill-id/mental-shield ; buggy, temporary disabled
									))
									(else (list))
								)
								(list)
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
					
					(define follow (make-follower connection))
					(define travel (make-traveller connection))
					(define buff (make-buffer connection))
					
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
					
					(define (support-iterate)
						(let ((creature (object-ref world (@: me 'target-id))))
							(when creature
								(cond
									((and (hp-danger? creature) (skill-ready? (skill-ref world skill-id/battle-heal)))
										(use-skill connection skill-id/battle-heal)
									)
									((and (not (hp-almost-full? creature)) (skill-ready? (skill-ref world skill-id/heal)))
										(use-skill connection skill-id/heal)
									)
									((and (not (mp-almost-full? creature)) (skill-ready? (skill-ref world skill-id/recharge)))
										(use-skill connection skill-id/recharge)
									)
								)
							)
						)
					)
					
					(let loop ()
						(let ((event (sync events)))
							(case (if event (car event) #f)
								; custom events
								
								; standard events
								((skill-launched) (let-values (((object-id skill-id level) (apply values (cdr event))))
									(when (and (equal? object-id (@: me 'object-id)) (@: me 'target-id)) ; If subject is me and i have target
										(when (equal? state 'state/buffing) ; Process next buff
											(unless (buff) (set! state 'state/nothing))
										)
										(when (equal? state 'state/supporting)
											(support-iterate)
										)
									)
								))
								((skill-reused) (let-values (((object-id skill-id level) (apply values (cdr event))))
									(when (= object-id (@: me 'object-id)) (cond
										((and (equal? state 'state/supporting) (@: me 'target-id))
											(support-iterate)
										)
									))
								))
								((change-moving) (let-values (((object-id position destination) (apply values (cdr event))))
									(when (and (equal? state 'state/following) (equal? object-id (@: me 'target-id))) ; Follow if master
										(unless (follow) (set! state 'state/nothing))
									)
									(when (and (equal? state 'state/traveling) (equal? object-id (@: me 'object-id))) ; Travel next if it is me
										(when (and (not destination) (zero? (travel 'next)))
											(set! state 'state/nothing)
											(logout connection)
										)
									)
								))
								((creature-update) (let* ((object-id (second event)) (creature (object-ref world object-id)))
									(when (and creature (equal? state 'state/supporting) (equal? object-id (@: me 'target-id)))
										(support-iterate) ;(displayln "\n\n\n\n\n\n\n\n\n\nhere")
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
								((message) (let-values (((author-id channel author text) (apply values (cdr event))))
									(let ((command (parse-command text)))
										(define (get-targets command)
											(apply parse-targets (cons connection (cons (object-ref world author-id) command)))
										)
										(if command
											(case (car command)
												(("hello") (gesture connection 'gesture/hello))
												(("bye") (logout connection))
												(("buff") (let ((targets (get-targets (cdr command))))
(displayln (cons "targets" targets))
													(if (and targets (not (null? targets)))
														(if (buff (map (lambda (object-id) (cons object-id (get-buff-list object-id))) targets))
															(set! state 'state/buffing)
															(say connection "Nothing to buff")
														)
														(when (equal? state 'state/buffing) (buff))
													)
												))
												(("power") (auto-shot connection (not (string=? (try-second command "") "off")) 'blessed-spiritshot 'd))
												(("support")
													(target/sync connection author-id)
													(set! state 'state/supporting)
													(support-iterate)
												)
												(("resurrect")
													(target/sync connection (try-first (get-targets (cdr command))))
													(use-skill connection skill-id/resurrection)
												)
												(("relax") (relax))
												(("follow")
													(if (follow (try-first (get-targets (cdr command))))
														(set! state 'state/following)
														(say connection "I don't see a target")
													)
												)
												(("travel") (if (null? (cdr command)) ; TODO feedback if died and when finished
													(say connection (format "~a points left" (travel 'left)) author)
													(let ((count (travel (cond
															((= (length command) 4) (apply point/3d (map string->number (cdr command)))) ; To location
															((= (length command) 2) (second command)) ; To known place ; TODO use DB
														))))
														(when (> count 0)
															(set! state 'state/traveling)
															(say connection (format "Traveling through ~a points" count) author)
														)
													)
												))
												; TODO use item-name
												; TODO say {info}
												; TODO reborn: town, clanhall, ...
												; TODO route: record/finish
												(("return") (return author-id))
												(else (say connection "I don't understand"))
											)
											(displayln (format-chat-message author-id channel author text))
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
