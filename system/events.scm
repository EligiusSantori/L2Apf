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
			#|
			((#x01) ; CharMoveToLocation
				; (update-character ... object)
			)
			((#x03) ; CharInfo
				; (update-antogonist object)
			)
			((#x04) ; UserInfo
				; (update-protoganist object)
				; register-object!
			)
			|#
			;((#x05) ; Attack
			;	
			;)
			
			;((#x06) ; Die
			;
			;)
			;((#x07) ; Revive
			;	
			;)
			
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
			((#x12) ; DeleteObject
				; (discard-object! object world)
			)
			
			((#x16) ; NpcInfo
				; (let npc from world; (update-npc! npc struct)
			)
			|#
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
			#|
			((#x47) ; StopMove
			
			)
			|#
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
				(make-event 'unhandled-packet (list
					(cons 'id (get-packet-id buffer))
				))
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
				(filter (lambda (b) (if e ((second b) e) #f)) l)
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