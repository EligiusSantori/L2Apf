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

(define skill-id/stun-attack 100)
(define skill-id/wild-sweep 245)
(define skill-id/spoil 254)
(define skill-id/sweep 42)
(define states (list
	'state/fighting
	'state/escaping
	'state/resting
	'state/nothing
	'state/following
	'state/traveling
))

(define (artisan? me)
	(member (@: me 'class-id) (list 56 57 118))
)
(define (scavenger? me)
	(member (@: me 'class-id) (list 54 55 117))
)
(define (hp-danger? me)
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
)

; TODO traveling
; TODO escaping
; TODO following
; TODO autologout

(let-values (((account password host port name) (parse-protocol (current-command-line-arguments))))
	(let ((connection (connect host port)))
		(let ((world (first (login connection account password))))
			(let ((me (@: (select-server connection world) name)))
				(let ((events (select-character connection me)))
				
					(define state 'state/nothing)
					
					(define (stun-attack-pointful?)
						(and ; TODO mp cost
							(@: me 'target-id)
							(equal? state 'state/fighting)
							(or (not (mp-economy? me)) (hp-danger? me))
							(skill-ready? (skill-ref world skill-id/stun-attack))
						)
					)
					(define (wild-sweep-pointful?)
						(and ; TODO mp cost
							(@: me 'target-id)
							(equal? state 'state/fighting)
							(or (not (mp-economy? me)) (hp-danger? me))
							(skill-ready? (skill-ref world skill-id/wild-sweep))
						)
					)
					(define (spoil-pointful?)
						(let ((creature (object-ref world (@: me 'target-id))))
							(and
								(npc? creature)
								(not (@: creature 'spoiled?))
								(not (@: creature 'alike-dead?))
								(equal? state 'state/fighting)
								(skill-ready? (skill-ref world skill-id/spoil))
							)
						)
					)
					(define (sweep-pointful?)
						(let ((creature (object-ref world (@: me 'target-id))))
							(and
								(npc? creature)
								(@: creature 'spoiled?)
								(@: creature 'alike-dead?)
								(skill-ready? (skill-ref world skill-id/sweep))
							)
						)
					)
				
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
					
					(define (assist master-id)
						(let ((whose (object-ref world master-id)))
							(let ((target-id (if whose (@: whose 'target-id) #f)))
								(cond
									((not target-id) (say connection "I don't see a target"))
									((equal? target-id (@: me 'object-id)) (say connection "It's me"))
									(else
										(set! state 'state/fighting)
										(when (not (equal? target-id (@: me 'target-id)))
											(target/sync connection target-id)
										)
										(when (@: me 'sitting?)
											(sit connection #f)
										)
										(cond
											((artisan? me) (if (stun-attack-pointful?)
												(use-skill connection skill-id/stun-attack)
												(attack connection)
											))
											((scavenger? me) (cond
												((sweep-pointful?) (use-skill connection skill-id/sweep)) ; If dead and spoiled use sweep
												((spoil-pointful?) (use-skill connection skill-id/spoil)) ; Start attack with spoil
												((wild-sweep-pointful?) (use-skill connection skill-id/wild-sweep)) ; Else attack with skill
												(else (attack connection)) ; Else attack with hand
											))
											(else (attack connection))
										)
									)
								)
							)
						)
					)
					
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
							(when (and (not sitting?) (or (< hp max-hp) (< mp max-mp)))
								(sit connection #t)
							)
						)
					)
					
					;(define (follow))
					
					;(define (travel))
					
					(define (escape)
						(set! state 'state/escaping)
						(say connection "Help me")
						;(follow ...)
					)
					
					(let loop ()
						(let ((event (sync events)))
							(case (if event (car event) #f)
								; custom events
								
								; standard events
								((change-moving) (let-values (((object-id position destination) (apply values (cdr event))))
									(when (and (equal? state 'state/following) (equal? object-id (@: me 'target-id))) ; follow if master
										(move-to connection (or destination position))
									)
									; todo check is finished
									(when (and (equal? state 'state/traveling) (equal? object-id (@: me 'object-id))) ; travel next if it is me
										(when (zero? (traveller 'next))
											(set! state 'state/nothing)
										)
									)
								))
								((skill-reused) (let-values (((object-id skill-id level) (apply values (cdr event))))
									(when (= object-id (@: me 'object-id)) (cond
										((and (= skill-id skill-id/stun-attack) (stun-attack-pointful?))
											(use-skill connection skill-id/stun-attack)
										)
										((or (= skill-id skill-id/spoil) (= skill-id skill-id/wild-sweep)) (cond
											((spoil-pointful?) (use-skill connection skill-id/spoil))
											((wild-sweep-pointful?) (use-skill connection skill-id/wild-sweep))
										))
									))
								))
								; TODO skill failed => attack
								((die) (let-values (((object-id return) (apply values (cdr event))))
									(when (equal? object-id (@: me 'target-id))
										(sleep 1/3)
										(let ((target (object-ref world object-id)))
											(if (and target (scavenger? me) (@: target 'spoiled?))
												(use-skill connection skill-id/sweep)
												(relax) ; TODO autorelax when monster die and if no agressive lock party members in near
											)
										)
									)
								))
								((ask) (let-values (((question data) (apply values (cdr event))))
									(reply connection question (equal? question 'ask/join-party))
								))
								((message) (let-values (((object-id channel author text) (apply values (cdr event))))
									(let ((command (parse-command text)))
										(if command
											(case (car command)
												(("hello") (gesture connection 'gesture/hello))
												(("bye") (logout connection))
												;(("travel") ...)
												(("assist") (assist object-id))
												(("follow") (follow object-id))
												(("return") (return object-id))
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
												(("who") (cond
													((artisan? me) (say connection "I'm an artisan"))
													((scavenger? me) (say connection "I'm an scavenger"))
													(else (say connection "I'm an dwarf"))
												))
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