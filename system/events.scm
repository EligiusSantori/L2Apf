(module system racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
		racket/async-channel
		"../library/structure.scm"
		(only-in "../library/network.scm" get-packet-id disconnect)
		"../logic/main.scm"
		"../logic/world.scm"
		"../logic/object.scm"
		"../logic/creature.scm"
		"../logic/npc.scm"
		"../logic/character.scm"
		"../logic/antagonist.scm"
		"../logic/protagonist.scm"
		"../packet/game/server/chat_message.scm"
		"../packet/game/server/social_action.scm"
		"../packet/game/server/die.scm"
		"../packet/game/server/revive.scm"
		"../packet/game/server/object_deleted.scm"
		"../packet/game/server/user_info.scm"
		"../packet/game/server/char_info.scm"
		"../packet/game/server/npc_info.scm"
		"../packet/game/server/move_to_point.scm"
		"../packet/game/server/stop_moving.scm"
		"../packet/game/server/status_update.scm"
		"../packet/game/server/target_selected.scm"
		"../packet/game/server/target_unselected.scm"
		"../packet/game/server/ask_join_alliance.scm"
		"../packet/game/server/ask_join_clan.scm"
		"../packet/game/server/ask_join_party.scm"
		"../packet/game/server/ask_be_friends.scm"
		"make_event.scm"
	)

	(provide (contract-out
		(set-event! (->* (box? symbol? (or/c procedure? false/c)) #:rest (or/c false/c (listof any/c)) void?))
		(run-event (box? symbol? list? . -> . void?))
		(make-event-channel (box? . -> . evt?))
	))
	
	(define (set-event! connection name checker . tail)
		(define handler (if (null? tail) values (car tail)))
		(let ((events (alist-delete name (@: connection 'events))))
			(set-box-field! connection 'events
				(if checker (cons (list name checker handler) events) events)
			)
			(void)
		)
	)
	
	(define (set-proxy-event! connection name . tail)
		(define handler (if (null? tail) values (car tail)))
		(define (checker event) (equal? name (@: 'name event)))

		(let ((proxy (gensym)))
			(set-event! connection proxy checker handler)
			proxy
		)
	)
	
	(define (run-event connection name data)
		(let ((channel (@: connection 'custom-channel)))
			(async-channel-put channel (make-event name data))
			(void)
		)
	)
	
	(define (handle-packet buffer world)
		(case (get-packet-id buffer)
			((#x01) (let ((packet (game-server-packet/move-to-point buffer)))
				(let ((object-id (@: packet 'object-id)) (p (@: packet 'position)) (d (@: packet 'destination)))
					(let ((moving? (not (equal? p d))))
						(define struct (list
							(cons 'position p)
							(cons 'destination d)
							(cons 'moving? moving?)
						))
						(update-creature!
							(get-object world object-id)
							(if moving? (alist-cons 'angle (points-angle p d) struct) struct)
						)
						
						(make-event 'change-moving (list
							(cons 'object-id object-id)
							(cons 'action (if moving? 'start 'stop))
						))
					)
				)
			))
			((#x03) (let ((packet (game-server-packet/char-info buffer)))
				(let ((antagonist (create-antagonist packet)))
					(register-object! world antagonist)
					(make-event 'creature-info (list
						(cons 'object-id (@: antagonist 'object-id))
					))
				)
			))
			((#x04) (let ((packet (game-server-packet/user-info buffer)))
				(let ((me (@: world 'me)))
					(update-protagonist! me packet)
					(register-object! world me)
					(make-event 'creature-update (list
						(cons 'object-id (@: me 'object-id))
					))
				)
			))
			;((#x05) ; Attack
			;	
			;)
			((#x06) (let ((packet (game-server-packet/die buffer)))
				(let ((creature (get-object world (@: packet 'object-id))) (spoiled? (@: packet 'spoiled?)))
					(let ((data (list (cons 'moving? #f) (cons 'alike-dead? #t))))
						(cond
							((and (protagonist? creature) (<= 0 (@: creature 'hp)))
								(update-protagonist! creature (alist-cons 'died? #t data))
							)
							((npc? creature)
								(update-npc! creature (alist-cons 'spoiled? spoiled? data))
							)
							(else (update-creature! creature data))
						)
					)
				)
				
				(make-event 'die (list
					(cons 'object-id (@: packet 'object-id))
					(cons 'return (@: packet 'return))
				))
			))
			((#x07) (let ((packet (game-server-packet/revive buffer)))
				(let ((creature (get-object world (@: packet 'object-id))))
					(if (protagonist? creature)
						(update-protagonist! creature (list (cons 'died? #f) (cons 'alike-dead? #f)))
						(update-creature! creature (list (cons 'alike-dead? #f)))
					)
				)
			
				(make-event 'revive (list
					(cons 'object-id (@: packet 'object-id))
				))
			))
			;((#x0b) ; SpawnItem
			;	; (register-object item world)
			;)
			;((#x0c) ; DropItem
			;	
			;)
			;((#x0d) ; GetItem
			;	
			;)
			((#x0e) (let ((packet (game-server-packet/status-update buffer)))
				(let ((creature (get-object world (@: packet 'object-id))))
					(cond
						((protagonist? creature) (update-protagonist! creature packet))
						((antagonist? creature) (update-antagonist! creature packet))
						((npc? creature) (update-npc! creature packet))
					)
				)
			
				(make-event 'creature-update (list
					(cons 'object-id (@: packet 'object-id))
				))
			))
			((#x12) (let ((packet (game-server-packet/object-deleted buffer)))
				(let ((object (get-object world (@: packet 'object-id))))
					(discard-object! world object)
					(make-event 'object-delete (list
						(cons 'object object) ; TODO return object-id but discard in next cycle
					))
				)
			))
			((#x16) (let ((packet (game-server-packet/npc-info buffer)))
				(let ((npc (create-npc packet)))
					(register-object! world npc)
					(make-event 'creature-info (list
						(cons 'object-id (@: npc 'object-id))
					))
				)
			))
			;((#x1b) ; ItemList
			;	(void)
			;)
			((#x29) (let ((packet (game-server-packet/target-selected buffer)))
				(let ((creature (get-object world (@: packet 'object-id))))
					(update-creature! creature (list
						(cons 'target-id (@: packet 'target-id))
						(cons 'position (@: packet 'position))
					))
				)
				
				(make-event 'change-target (list
					(cons 'object-id (@: packet 'object-id))
					(cons 'target-id (@: packet 'target-id))
				))
			))
			((#x2a) (let ((packet (game-server-packet/target-unselected buffer)))
				(let ((creature (get-object world (@: packet 'object-id))))
					(update-creature! creature (list
						(cons 'target-id #f)
						(cons 'position (@: packet 'position))
					))
				)
				
				(make-event 'change-target (list
					(cons 'object-id (@: packet 'object-id))
					(cons 'target-id #f)
				))
			))
			((#x2d) (let ((packet (game-server-packet/social-action buffer)))
				(make-event 'social-action (list
					(cons 'object-id (@: packet 'object-id))
					(cons 'action (@: packet 'action))
				))
			))
			((#x32) (let ((packet (game-server-packet/ask-join-clan buffer)))
				(make-event 'ask (list
					(cons 'question 'ask/join-clan)
					(cons 'clan (@: packet 'name))
				))
			))
			((#x39) (let ((packet (game-server-packet/ask-join-party buffer)))
				(make-event 'ask (list
					(cons 'question 'ask/join-party)
					(cons 'player (@: packet 'name))
					(cons 'loot (@: packet 'loot))
				))
			))
			;((#x45) ; ShortcutInit
			;	(void)
			;)
			((#x47) (let ((packet (game-server-packet/stop-moving buffer)))
				(let ((object-id (@: packet 'object-id)) (p (@: packet 'position)) (a (@: packet 'angle)))
					(update-creature! (get-object world object-id) (list
						(cons 'angle a)
						(cons 'position p)
						(cons 'destination p)
						(cons 'moving? #f)
					))
					
					(make-event 'change-moving (list
						(cons 'object-id object-id)
						(cons 'action 'stop)
					))
				)
			))
			((#x4a) (let ((packet (game-server-packet/chat-message buffer)))
				(make-event 'message (list
					(cons 'object-id (@: packet 'object-id))
					(cons 'channel (@: packet 'channel))
					(cons 'author (@: packet 'author))
					(cons 'text (@: packet 'text))
				))
			))
			;((#x58) ; SkillList
			;	(void)
			;)
			;((#x64) ; SystemMessage
			;	(void)
			;)
			((#x7d) (let ((packet (game-server-packet/ask-be-friends buffer)))
				(make-event 'ask (list
					(cons 'question 'ask/be-friends)
					(cons 'player (@: packet 'name))
				))
			))
			((#x7e) ; logout
				(make-event 'logout)
			)
			;((#x7f) ; ?
			;	(void)
			;)
			;((#x80) ; QuestList
			;	(void)
			;)
			((#xa8) (let ((packet (game-server-packet/ask-join-alliance buffer)))
				(make-event 'ask (list
					(cons 'question 'ask/join-alliance)
					(cons 'alliance (@: packet 'name))
				))
			))
			;((#xe4) ; HennaInfo
			;	(void)
			;)
			;((#xe7) ; SendMacroList
			;	(void)
			;)
			;((#xf8) ; SignsSky
			;	(void)
			;)
			;((#xfe) ; Ex*
			;	(void)
			;)
			(else
				(displayln (format "unhandled packet #~x" (get-packet-id buffer)))
				(make-event 'nothing)
			)
		)
	)
	
	(define (make-network-channel connection)
		(let ((world (@: connection 'world)))
			(wrap-evt
				(@: connection 'input-channel)
				(lambda (buffer) (handle-packet buffer world))
			)
		)
	)
	
	(define (make-event-channel connection)
		(let ((world (@: connection 'world)) (custom-channel (make-async-channel)))
			(define (handle b e)
				(let ((c (make-event (first b) ((third b) e)))) ; handle
					(async-channel-put custom-channel c) 
					e
				)
			)
			(define (check e l)
				(filter (lambda (b) ((second b) e)) l)
			)
		
			(set-box-field! connection 'custom-channel custom-channel) ; custom events channel
			(set-box-field! connection 'events (list)) ; custom events list
		
			(set-event! connection (gensym) ; disconnect via custom events
				(lambda (e) (equal? (@: e 'name ) 'logout))
				(lambda (e) (begin (disconnect connection) #f))
			)
		
			(wrap-evt
				(choice-evt
					custom-channel
					(@: connection 'time-channel)
					(make-network-channel connection)
				)
				(lambda (event)
					(let ((events (@: connection 'events)))
						(fold handle event (check event events)) ; initiate custom events and return original
					)
				)
			)
		)
	)
)
