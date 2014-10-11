(module system racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
		racket/async-channel
		"../library/structure.scm"
		(only-in "../library/network.scm" get-packet-id disconnect)
		"../library/logic.scm"
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
					(let ((a (points-angle p d)) (moving? (not (equal? p d))))
						(define struct (list
							(cons 'position p)
							(cons 'destination d)
							(cons 'moving? moving?)
						))
						(update-creature!
							(get-object world object-id)
							(if a (alist-cons 'angle a struct) struct)
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
				(let ((me (get-object world 'me)))
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
			
			((#x06) (let ((packet (game-server-packet/die buffer))) ; TODO toggle object state
				(make-event 'die (list
					(cons 'object-id (@: packet 'object-id))
					(cons 'spoiled? (@: packet 'spoiled?))
					(cons 'return (@: packet 'return))
				))
			))
			((#x07) (let ((packet (game-server-packet/revive buffer))) ; TODO toggle object state
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
			
			#|
			((#x0e) ; StatusUpdate
			
			)
			|#
			
			((#x12) (let ((packet (game-server-packet/object-deleted buffer)))
				(let ((object-id (@: packet 'object-id)))
					(make-event 'object-deleted (list
						(cons 'object-id object-id)
					))
					(discard-object! world object-id)
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
			
			;((#x29) ; TargetSelected
			;	; (set-target! object target)
			;)
			;((#x2a) ; TargetUnselected
			;	; (set-target! object #f)
			;)
			
			((#x2d) (let ((packet (game-server-packet/social-action buffer)))
				(make-event 'social-action (list
					(cons 'object-id (@: packet 'object-id))
					(cons 'action (@: packet 'action))
				)
			)))
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
			((#x7e) ; logout
				(make-event 'logout)
			)
			;((#x7f) ; ?
			;	(void)
			;)
			;((#x80) ; QuestList
			;	(void)
			;)
			;((#xa6) ; MyTargetSelected
			;	
			;)
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
				(lambda (buffer)
					(handle-packet buffer world)
				)
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
