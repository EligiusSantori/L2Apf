(module system racket/base ; Intermediate layer between low-level protocol and user API to provide higher-level events and state of game world.
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
		racket/function
		racket/async-channel
		"debug.scm"
		"structure.scm"
		"connection.scm"
		"event.scm"
		(only-in "../packet/packet.scm" get-packet-id)
		(relative-in "../packet/game/server/."
			"chat_message.scm"
			"system_message.scm"
			"social_action.scm"
			"attack.scm"
			"die.scm"
			"revive.scm"
			"object_deleted.scm"
			"user_info.scm"
			"char_info.scm"
			"npc_info.scm"
			"move_to_point.scm"
			"move_to_pawn.scm"
			"stop_moving.scm"
			"status_update.scm"
			"target_selected.scm"
			"target_unselected.scm"
			"ask_join_alliance.scm"
			"ask_join_clan.scm"
			"ask_join_party.scm"
			"ask_be_friends.scm"
			"change_move_type.scm"
			"change_wait_type.scm"
			"skill_list.scm"
			"skill_started.scm"
			"skill_launched.scm"
			"skill_canceled.scm"
			"party_member_update.scm"
			"teleport.scm"
			"spawn_item.scm"
		)
		(relative-in "../packet/game/client/."
			"validate_position.scm"
			"appearing.scm"
		)
		(relative-in "../model/."
			"map.scm"
			"world.scm"
			"object.scm"
			"creature.scm"
			"npc.scm"
			"character.scm"
			"antagonist.scm"
			"protagonist.scm"
			; "quest.scm"
			"skill.scm"
			"item.scm"
		)
	)
	(provide (contract-out
		(handle-interrupt (-> connection? async-channel? async-channel? async-channel? event?))
	))

	(define (trigger! ec name . data) (async-channel-put ec (apply make-event (cons name data))) name); async-channel? symbol? any/c ...
	; (define (trigger-change-moving! cn object-id from to) (trigger! cn 'change-moving object-id from to)) ; connection? point? point?
	; (define (trigger-creature-create! cn creature) (trigger! cn 'creature-create creature)) ; connection? creature?

	(define (handle-interrupt cn ec pc tc)
		(let ((wr (connection-world cn)) (evt (sync/enable-break ec pc (connection-read-thread cn))))
			(cond
				((bytes? evt)
					(let ((row (hash-ref handlers-table (get-packet-id evt) #f))) ; Incoming packet => handle then wait next.
						(if row
							((cdr row) wr ((car row) evt) ec)
							(apf-warn "Unhandled packet ~v" evt)
						)
						(handle-interrupt cn ec pc tc)
					)
				)
				((thread? evt) (make-event 'disconnect)) ; Connection closed => return immediately.
				(else
					(apf-debug "Event ~a" evt)
					(case (event-name evt) ; Regular event => postprocess or just return.
						((logout) ; Close socket.
							(disconnect cn)
							evt
						)
						((object-delete) (let ((object-id (second evt)))
							(async-channel-put tc (lambda () ; Actually after event processing.
								(discard-object! wr object-id)
							))
							evt
						))
						((teleport) (let* ((me (world-me wr)) (position (ref me 'position)) (angle (ref me 'angle))) ; Request updates. ; TODO handle this in input loop
							(send-packet cn (game-client-packet/validate-position position angle))
							(send-packet cn (game-client-packet/appearing))
							evt
						))
						; ((attack) (let* ((object-id (second e)) (creature (object-ref world object-id)) (target (...))) ; Fix position refreshing event on attack.
						; 	(when creature (trigger-change-moving! cn object-id (ref creature 'position) (ref target 'position)))
						; ))
						; (skill-started) ; Custom event on skill reused.
						; 	(when (= (second e) (ref (world-me wr) 'object-id))
						; 		(let-values (((name object-id skill-id level) (apply values e)))
						; 			(let ((skill (skill-ref wr skill-id)))
						; 				(when skill
						; 					(let ((last-usage (ref skill 'last-usage)) (reuse-delay (ref skill 'reuse-delay)))
						; 						(when (and reuse-delay (> reuse-delay 0))
						; 							(set-alarm! connection 'skill-reused (+ last-usage reuse-delay) object-id skill-id level)
						; 						)
						; 					)
						; 				)
						; 			)
						; 		)
						; 	)
						; 	evt
						; )
						(else evt)
					)
				)
			)
		)
	)

	(define (packet-handler/user-info wr packet ec)
		(let ((me (world-me wr)))
			(update-protagonist! me packet)
			(register-object! wr me)
			(trigger! ec 'creature-create (ref me 'object-id))
		)
	)
	(define (packet-handler/char-info wr packet ec)
		(let ((antagonist (create-antagonist packet)))
			(register-object! wr antagonist)
			(trigger! ec 'creature-create (ref antagonist 'object-id))
		)
	)
	(define (packet-handler/npc-info wr packet ec)
		(let ((npc (create-npc packet)))
			(register-object! wr npc)
			(trigger! ec 'creature-create (ref npc 'object-id))
		)
	)
	(define (packet-handler/status-update wr packet ec)
		(let* ((object-id (ref packet 'object-id)) (creature (object-ref wr object-id)))
			(cond
				((protagonist? creature) (update-protagonist! creature packet))
				((antagonist? creature) (update-antagonist! creature packet))
				((npc? creature) (update-npc! creature packet))
			)
			(trigger! ec 'creature-update object-id) ; TODO updates
		)
	)
	(define (packet-handler/object-deleted wr packet ec)
		(let* ((object-id (ref packet 'object-id)) (object (object-ref wr object-id)))
			(when (creature? object) ; Clear targets for aimed to deleted object
				(objects wr (lambda (subject)
					(when (and (creature? subject) (equal? object-id (ref subject 'target-id)))
						(update-creature! subject (list (cons 'target-id #f)))
					)
				))
			)
			(trigger! ec 'object-delete object-id)
		)
	)

	(define (packet-handler/skill-list wr packet ec)
		(let ((skills (world-skills wr)))
			(hash-clear! skills)
			(apply hash-set*! skills (apply append (map (lambda (i)
				(let ((skill-id (ref i 'skill-id)) (level (ref i 'level)) (active? (ref i 'active?)))
					(list skill-id (make-skill skill-id level active?))
				)
			) (ref packet 'list))))

			(trigger! ec 'skills-update (ref (world-me wr) 'object-id))
		)
	)

	(define (packet-handler/chat-message wr packet ec)
		(trigger! ec 'message
			(ref packet 'object-id)
			(ref packet 'channel)
			(ref packet 'author)
			(ref packet 'text)
		)
	)
	(define (packet-handler/system-message wr packet ec)
		(let ((message-id (ref packet 'message-id)) (arguments (ref packet 'arguments)))
			(when (or (= message-id 612) (= message-id 357)) ; Set spoiled state here bacause there is no other way.
				(let ((target-id (ref (world-me wr) 'target-id)))
					(update-npc! (object-ref wr target-id) (list
						(cons 'spoiled? #t)
					))
				)
			)

			; TODO Set is effect failed?

			(apply trigger! ec 'system-message message-id arguments)
		)
	)

	(define (packet-handler/social-action wr packet ec)
		(trigger! ec 'gesture
			(ref packet 'object-id)
			(ref packet 'action)
		)
	)

	(define (packet-handler/logout wr packet ec)
		(trigger! ec 'logout)
	)

	; TODO rewrite handlers

	(define (packet-handler/move-to-point world packet)
		(let ((object-id (@: packet 'object-id)) (p (@: packet 'position)) (d (@: packet 'destination)))
			(let ((moving? (not (equal? p d))) (creature (object-ref world object-id)))
				(when creature (update-creature! creature (list
					(cons 'position p)
					(cons 'destination (if moving? d #f))
					(if moving?
						(cons 'angle (points-angle p d))
						#f
					)
				)))
				(values object-id p (if moving? d #f))
			)
		)
	)
	(define (packet-handler/move-to-pawn world packet) ; Происходит, когда игрок бежит не к точке, а к монстру или NPC
		(let* ((id (@: packet 'object-id)) (target-id (@: packet 'target-id)) (position (@: packet 'position)))
			(let* ((creature (object-ref world id)) (target (object-ref world target-id)))
				(if (and creature target)
					(let* ((destination (@: target 'position)) (moving? (not (equal? position destination))))
						(update-creature! creature (list
							(cons 'position position)
							(cons 'destination (moving? destination #f))
							(if moving?
								(cons 'angle (points-angle position destination))
								#f
							)
						))
						(values id position (if moving? destination #f))
					)
					(begin
						(apf-warn "World objects ~e is missed, causer {packet:move-to-pawn}."
							(map object-id (filter identity creature target))
						)
						(values id position #f) ; TODO omit event and log warning
					)
				)
			)
		)
	)
	(define (packet-handler/stop-moving world packet)
		(let* ((object-id (@: packet 'object-id)) (p (@: packet 'position)) (a (@: packet 'angle)))
			(let ((creature (object-ref world object-id))) (when creature (update-creature! creature (list
				(cons 'angle a)
				(cons 'position p)
				(cons 'destination #f)
			))))
			(values object-id p #f)
		)
	)

	(define (packet-handler/change-move-type world packet)
		(let* ((object-id (@: packet 'object-id)) (creature (object-ref world object-id)))
			(when (creature? creature) (update-creature! creature packet))
			(values object-id)
		)
	)

	(define (packet-handler/change-wait-type world packet)
		(let* ((object-id (@: packet 'object-id)) (creature (object-ref world object-id)))
			(when (creature? creature) (update-creature! creature packet))
			(values object-id)
		)
	)

	(define (packet-handler/attack world packet)
		(let* ((object-id (@: packet 'object-id)) (creature (object-ref world object-id)))
			(let* ((hits (@: packet 'hits)) (target-id (@: (car hits) 'target-id)))
				(when creature
					(update-creature! creature (list
						(cons 'target-id target-id)
						(cons 'position (@: packet 'position))
						(cons 'destination #f)
					))
					; TODO
						; ? substract damage from hp
						; ? refresh in-combat state for self and all targets if not miss
				)
				(values object-id target-id hits)
			)
		)
	)

	(define (packet-handler/die world packet)
		(let* ((object-id (@: packet 'object-id)) (creature (object-ref world object-id)) (spoiled? (@: packet 'spoiled?)))
			(let ((data (list (cons 'destination #f) (cons 'alike-dead? #t))))
				(cond
					((and (protagonist? creature) (<= 0 (@: creature 'hp)))
						(update-protagonist! creature (alist-cons 'died? #t data))
					)
					((npc? creature)
						(update-npc! creature (alist-cons 'spoiled? spoiled? data))
					)
					(creature
						(update-creature! creature data)
					)
				)
			)
			(let ((return (if (equal? object-id (@: world 'me 'object-id)) (@: packet 'return) #f)))
				(values object-id return)
			)
		)
	)

	(define (packet-handler/revive world packet)
		(let* ((object-id (@: packet 'object-id)) (creature (object-ref world object-id)))
			(cond
				((protagonist? creature)
					(update-protagonist! creature (list (cons 'died? #f) (cons 'alike-dead? #f)))
				)
				(creature
					(update-creature! creature (list (cons 'alike-dead? #f)))
				)
			)
			(values object-id)
		)
	)

	(define (packet-handler/target-selected world packet)
		(let* ((object-id (@: packet 'object-id)) (target-id (@: packet 'target-id)) (position (@: packet 'position)))
			(let ((creature (object-ref world object-id)))
				(when creature (update-creature! creature (list
					(cons 'target-id target-id)
					(cons 'position position)
					(if (equal? position (@: creature 'destination))
						(cons 'destination #f)
						#f
					)
				)))
				(values object-id target-id)
			)
		)
	)

	(define (packet-handler/target-unselected world packet)
		(let* ((object-id (@: packet 'object-id)) (creature (object-ref world object-id)))
			(when creature (update-creature! creature (list
				(cons 'target-id #f)
				(cons 'position (@: packet 'position))
				(if (equal? (@: packet 'position) (@: creature 'destination))
					(cons 'destination #f)
					#f
				)
			)))
			(values object-id #f)
		)
	)

	(define (packet-handler/ask-join-clan world packet)
		(values 'ask/join-clan (list
			(cons 'clan (@: packet 'name))
		))
	)

	(define (packet-handler/ask-join-party world packet)
		(values 'ask/join-party (list
			(cons 'player (@: packet 'name))
			(cons 'loot (@: packet 'loot))
		))
	)

	(define (packet-handler/ask-be-friends world packet)
		(values 'ask/be-friends (list
			(cons 'player (@: packet 'name))
		))
	)

	(define (packet-handler/ask-join-alliance world packet)
		(values 'ask/join-alliance (list
			(cons 'alliance (@: packet 'name))
		))
	)

	(define (packet-handler/skill-started world packet)
		(let ((object-id (@: packet 'object-id)) (skill-id (@: packet 'skill-id)) (level (@: packet 'level)))
			(let ((creature (object-ref world object-id))) (when creature (update-creature! creature (list
				(cons 'target-id (@: packet 'target-id))
				(cons 'position (@: packet 'position))
				(if (equal? (@: packet 'position) (@: creature 'destination))
					(cons 'destination #f)
					#f
				)
				(cons 'casting? #t)
			))))

			(let* ((skills (@: world 'skills)) (skill (hash-ref skills skill-id #f)))
				(when skill
					(hash-set! skills skill-id (struct-transfer skill
						(list
							(cons 'last-usage (current-milliseconds)) ; (@: packet 'cast-origin)
							(cons 'reuse-delay (@: packet 'reuse-delay))
						)
						'last-usage
						'reuse-delay
					))
				)
			)

			(values object-id skill-id level)
		)
	)

	(define (packet-handler/skill-launched world packet)
		(let* ((object-id (@: packet 'object-id)) (skill-id (@: packet 'skill-id)) (level (@: packet 'level)) (creature (object-ref world object-id)))
			(when creature
				(update-creature! creature (list
					(cons 'target-id (@: packet 'target-id))
					(cons 'casting? #f)
				))
			)
			(values object-id skill-id level)
		)
	)

	(define (packet-handler/skill-canceled world packet)
		(let* ((object-id (@: packet 'object-id)) (creature (object-ref world object-id)))
			(when creature (update-creature! creature (list (cons 'casting? #f))))
			(values object-id)
		)
	)

	(define (packet-handler/party-member-update world packet)
		(let* ((object-id (@: packet 'object-id)) (creature (object-ref world object-id)))
			(when (character? creature)
				(update-character! creature packet)
			)
			(values object-id)
		)
	)

	; TODO Don't remove all objects, only those who too far or too long without updates and not in party?
	(define (packet-handler/teleport world packet)
		(let ((object-id (@: packet 'object-id)) (position (@: packet 'position)))
			(let ((creature (object-ref world object-id))) (when creature
				(update-creature! creature (list
					(cons 'position position)
					(cons 'destination #f)
				))

				(when (equal? object-id (@: world 'me 'object-id))
					(map ; Remove all known objects except myself.
						(lambda (object)
							(let ((object-id (@: object 'object-id)))
								;(trigger-event connection 'object-delete object-id)
								(discard-object! world object-id)
							)
						)
						(objects world (lambda (object)
							(not (equal? object-id (@: object 'object-id)))
						))
					)
				)
			))

			(values object-id position)
		)
	)

	(define (packet-handler/spawn-item world packet)
		(let ((object-id (@: packet 'object-id)) (item (create-item packet)))
			(register-object! world item)
			(values object-id)
		)
	)

	(define handlers-table (make-hash (list
		(cons #x01 (cons game-server-packet/move-to-point packet-handler/move-to-point)) ; change-moving
		(cons #x03 (cons game-server-packet/char-info packet-handler/char-info))
		(cons #x04 (cons game-server-packet/user-info packet-handler/user-info))
		(cons #x05 (cons game-server-packet/attack packet-handler/attack)) ; attack
		(cons #x06 (cons game-server-packet/die packet-handler/die)) ; die
		(cons #x07 (cons game-server-packet/revive packet-handler/revive)) ; revive
		; #x0a AttackCanceld
		(cons #x0b (cons game-server-packet/spawn-item packet-handler/spawn-item)) ; item-create
		;(cons #x0c (cons game-server-packet/drop-item packet-handler/drop-item)) ; DropItem
		;(cons #x0d (cons game-server-packet/get-item packet-handler/get-item))
		(cons #x0e (cons game-server-packet/status-update packet-handler/status-update))
		; #x10 SellList
		(cons #x12 (cons game-server-packet/object-deleted packet-handler/object-deleted))
		; #x13 CharSelectInfo
		; #x15 CharSelected
		; #x17 CharTemplates
		(cons #x16 (cons game-server-packet/npc-info packet-handler/npc-info))
		;(cons #x1b (cons game-server-packet/item-list packet-handler/item-list))
		; #x25 ActionFailed
		(cons #x28 (cons game-server-packet/teleport packet-handler/teleport)) ; teleport
		(cons #x29 (cons game-server-packet/target-selected packet-handler/target-selected)) ; change-target
		(cons #x2a (cons game-server-packet/target-unselected packet-handler/target-unselected)) ; change-target
		; #x2b ? AutoAttackStart
		; #x2c ? AutoAttackStop
		(cons #x2d (cons game-server-packet/social-action packet-handler/social-action))
		(cons #x2e (cons game-server-packet/change-move-type packet-handler/change-move-type)) ; creature-update
		(cons #x2f (cons game-server-packet/change-wait-type packet-handler/change-wait-type)) ; creature-update
		(cons #x32 (cons game-server-packet/ask-join-clan packet-handler/ask-join-clan)) ; ask
		(cons #x39 (cons game-server-packet/ask-join-party packet-handler/ask-join-party)) ; ask
		;(cons #x45 (cons game-server-packet/shortcut-init packet-handler/shortcut-init)) ; shortcut-list
		(cons #x47 (cons game-server-packet/stop-moving packet-handler/stop-moving)) ; change-moving
		(cons #x48 (cons game-server-packet/skill-started packet-handler/skill-started)) ; skill-started
		(cons #x49 (cons game-server-packet/skill-canceled packet-handler/skill-canceled)) ; skill-canceled
		(cons #x4a (cons game-server-packet/chat-message packet-handler/chat-message))
		; #x4b EquipUpdate
		; #x4c DoorInfo
		; #x4d DoorStatusUpdate
		; #x4e ?
		(cons #x52 (cons game-server-packet/party-member-update packet-handler/party-member-update)) ; creature-update
		(cons #x58 (cons game-server-packet/skill-list packet-handler/skill-list))
		(cons #x60 (cons game-server-packet/move-to-pawn packet-handler/move-to-pawn)) ; change-moving
		; #x61 ValidateLocation ; TODO
		(cons #x64 (cons game-server-packet/system-message packet-handler/system-message))
		; #x65 StartPledgeWar
		; #x6d SetupGauge
		; #x6f ChooseInventoryItem
		(cons #x76 (cons game-server-packet/skill-launched packet-handler/skill-launched)) ; skill-launched
		; #x76 SetToLocation
		(cons #x7d (cons game-server-packet/ask-be-friends packet-handler/ask-be-friends)) ; ask
		(cons #x7e (cons void packet-handler/logout)) ; logout
		;(cons #x7f (cons game-server-packet/ packet-handler/)) ; MagicEffectIcons
		;(cons #x80 (cons game-server-packet/quest-list packet-handler/quest-list)) ; quest-list
		; #x81 EnchantResult
		; #x86 Ride
		; #x98 PlaySound
		; #x99 StaticObject
		; #xa6 MyTargetSelected
		;(cons #xa7 ' (cons game-server-packet/ packet-handler/))
		(cons #xa8 (cons game-server-packet/ask-join-alliance packet-handler/ask-join-alliance)) ; ask
		; #xc4 Earthquake
		; #xc8 NormalCamera
		; #xd0 MultiSellList
		; #xd4 Dice
		; #xd5 Snoop
		;(cons #xe4 'henna-info (cons game-server-packet/henna-info packet-handler/henna-info))
		;(cons #xe7 'macro-list (cons game-server-packet/macro-list packet-handler/macro-list))
		; #xee PartySpelled
		; #xf5 SSQStatus
		;(cons #xf8 'signs-sky (cons game-server-packet/signs-sky packet-handler/signs-sky)) SignsSky
		;(cons #xfe 'Ex* (cons game-server-packet/ packet-handler/))

		;(cons #x?? game-server-packet/ packet-handler/)
	)))
)
