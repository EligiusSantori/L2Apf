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

(define skill-id/sleep 1069)
(define skill-id/slow 1160)
(define skill-id/shadow-flare 1267)
(define skill-id/hurricane 1239)
(define skill-id/surrender-to-wind 1074)
(define skill-id/corpse-life-drain 1151)

(define states (list
	'state/assisting
	'state/slaughtering
	'state/following
	'state/traveling
	;'state/escaping
	'state/resting
	'state/nothing
))

(define (get-attack-skill me)
	skill-id/hurricane ; TODO shadow-flare by cond
	
	#|(cond
		((member (@: me 'class-id) (list 56 57 118))
			skill-id/hurricance
		)
	)|#
)
(define (mp-critical? me)
	(let ((mp (@: me 'mp)) (max-mp (@: me 'max-mp)))
		(<= (/ mp max-mp) 1/10)
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

(let-values (((account password host port name) (parse-protocol (current-command-line-arguments))))
	(let ((connection (connect host port)))
		(let ((world (first (login connection account password))))
			(let ((me (@: (select-server connection world) name)))
				(let ((events (select-character connection me)))	
					(define state 'state/nothing) ; TODO complex component including stack feature
					(define response-to #f)
					
					(define (state-fighting?)
						(member state (list 'state/assisting 'state/slaughtering))
					)
					(define (attack-skill-pointful?)
						(and
							(@: me 'target-id)
							; TODO target is alive and hp > damage
							(state-fighting?)
							(not (mp-critical? me))
							(skill-ready? (skill-ref world skill-id/hurricane))
						)
					)
					(define (target-alive-other? subject)
						(and
							(creature? subject)
							(let* ((target-id (@: subject 'target-id)) (target (object-ref world target-id)))
								(and
									(creature? target)
									(not (object=? subject target))
									(not (@: target 'alike-dead?))
								)
							)
						)
					)

					(define follow (make-follower connection))
					(define travel (make-traveller connection))
					
					(define (assist master-id)
						(let ((whose (object-ref world master-id)))
							(if (target-alive-other? whose)
								(begin
									(set! state 'state/assisting)
									(set! response-to master-id)
									
									(let ((target-id (@: whose 'target-id)))
										(when (not (equal? target-id (@: me 'target-id)))
											(target connection target-id)
										)
									)
									(when (@: me 'sitting?)
										(sit connection #f)
									)
								)
								(say connection "Invalid target")
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
									(when (equal? object-id (@: me 'object-id))
										(if (and (equal? state 'state/slaughtering) (attack-skill-pointful?))
											(use-skill connection (get-attack-skill me)) ; If time to nuke then do it
											(let ((master (object-ref world response-to))) ; Else stay near master
												(when (and (state-fighting?) (creature? master))
													(move-to connection (or (@: master 'destination) (@: master 'position)))
												)
											)
										)
									)
								))
								((skill-reused) (let-values (((object-id skill-id level) (apply values (cdr event))))
									(when (and (equal? object-id (@: me 'object-id)) (equal? state 'state/slaughtering) (attack-skill-pointful?))
										(use-skill connection (get-attack-skill me))
									)
								))
								((die) (let-values (((object-id return) (apply values (cdr event))))
									(when (and (state-fighting?) (equal? object-id (@: me 'target-id)))
										(relax)
									)
									; TODO corpse-life-drain if health low
								))
								((change-moving) (let-values (((object-id position destination) (apply values (cdr event))))
									(when (and (state-fighting?) (equal? object-id response-to)) ; Stay near master
										(move-to connection (or destination position)) ; TODO gap 100
									)
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
												(("assist") (assist author-id))
												(("slaught")
													(if (target-alive-other? me)
														(begin
															(set! state 'state/slaughtering)
															(use-skill connection skill-id/surrender-to-wind)
														)
														(say connection "Invalid target")
													)
												)
												(("strike")
													(if (target-alive-other? me)
														(use-skill connection (get-attack-skill me))
														(say connection "Invalid target")
													)
												)
												(("power") (auto-shot connection (not (string=? (try-second command "") "off")) 'blessed-spiritshot 'd))
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