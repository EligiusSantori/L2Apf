#lang racket
(require
	srfi/1
	"library/extension.scm"
	"library/structure.scm"
	"system/contract.scm"
	"system/uri_scheme.scm"
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

(define skill-id/stun-attack 100)
(define skill-id/wild-sweep 245)
(define skill-id/spoil 254)
(define skill-id/sweep 42)
(define states (list
	'state/fighting
	'state/escaping
	'state/resting
	'state/nothing
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

(define (parse-config command-line)
	(if (> (vector-length command-line) 0)
		(let ((uri (parse-uri (vector-ref command-line 0))))
			(if uri
				(apply values uri)
				(error "Authentication failed because URI is broken")
			)
		)
		(error "Authentication failed because URI is missed")
	)
)

(define (parse-command text)
	(let ((t (string-trim (string-downcase text))))
		(if (string-starts? t "/")
			(let ((l (map string-trim (string-split t " "))))
				(cons (substring (car l) 1) (cdr l))
			)
			#f
		)
	)
)

(define (format-chat-message object-id channel author text)
	(let ((channel (string-titlecase (last (string-split (symbol->string channel) "/")))))
		(string-append "[" channel "] " author ": " text)
	)
)

(let-values (((account password host port name) (parse-config (current-command-line-arguments))))
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
												((spoil-pointful?) (use-skill connection skill-id/spoil))
												((wild-sweep-pointful?) (use-skill connection skill-id/wild-sweep))
												(else (attack connection))
											))
											(else (attack connection))
										)
									)
								)
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
								((die) (let-values (((object-id return) (apply values (cdr event))))
									(when (equal? object-id (@: me 'target-id))
										(sleep 1/3)
										(if (scavenger? me) ; TODO and when target spoiled?
											(use-skill connection skill-id/sweep)
											(relax) ; TODO autorelax when monster die and if no agressive lock party members in near
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
												(("return") (return object-id))
												(("relax") (relax))
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