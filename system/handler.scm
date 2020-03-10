(module system racket/base ; Intermediate layer between low-level protocol and user API to provide higher-level events and state of game world.
	(require
		srfi/1
		racket/function
		racket/set
		racket/async-channel
		(rename-in racket/contract (any all/c))
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
			"status_update.scm"
			"move_to_point.scm"
			"move_to_pawn.scm"
			"stop_moving.scm"
			"validate_location.scm"
			"target_selected.scm"
			"target_unselected.scm"
			"ask_join_alliance.scm"
			"ask_join_clan.scm"
			"ask_join_party.scm"
			"ask_be_friends.scm"
			"reply_join_party.scm"
			"change_move_type.scm"
			"change_wait_type.scm"
			"skill_list.scm"
			"skill_started.scm"
			"skill_launched.scm"
			"skill_canceled.scm"
			"party_all_members.scm"
			"party_add_member.scm"
			"party_member_update.scm"
			"party_member_position.scm"
			"party_delete_member.scm"
			"party_clear_members.scm"
			"teleport.scm"
			"spawn_item.scm"
		)
		(relative-in "../packet/game/client/."
			"validate_location.scm"
			"appearing.scm"
		)
		(relative-in "../model/."
			"object.scm"
			"item.scm"
			"skill.scm"
			; "quest.scm"
			"creature.scm"
			"npc.scm"
			"character.scm"
			"antagonist.scm"
			"protagonist.scm"
			"map.scm"
			"party.scm"
			"world.scm"
		)
	)
	(provide (contract-out
		(handle-interrupt (-> connection? async-channel? async-channel? async-channel? event?))
	))

	(define (trigger! ec name . data) (async-channel-put ec (apply make-event (cons name data))) name); async-channel? symbol? any/c ...
	; (define (trigger-change-moving! cn object-id from to) (trigger! cn 'change-moving object-id from to)) ; connection? point? point?
	; (define (trigger-creature-create! cn creature) (trigger! cn 'creature-create creature)) ; connection? creature?
	(define (handle-creature-update! ec wr data [creature #f])
		(let ((creature (or creature (object-ref wr (ref data 'object-id)))))
			(when creature
				(let* ((changes (update-creature! creature data)) (changed (apply seteq (map car changes))))
					(let ((change (assoc 'target-id changes eq?))) (when change
						(trigger! ec 'change-target (object-id creature) (cadr change))
					))
					(when (set-member? changed 'destination) ; Only on start or stop moving (skip position validation).
						(trigger! ec 'change-moving (object-id creature) (ref creature 'position) (ref creature 'destination))
					)
					(let ((change (assoc 'walking? changes eq?))) (when change
						(trigger! ec 'change-walking (object-id creature) (cadr change))
					))
					(let ((change (assoc 'sitting? changes eq?))) (when change
						(trigger! ec 'change-sitting (object-id creature) (cadr change))
					))
					(when (not (set-empty? (set-subtract changed (seteq 'target-id 'position 'destination 'angle 'walking? 'sitting?))))
						(trigger! ec 'creature-update (object-id creature) changes)
					)
				)
			)
			creature
		)
	)
	(define (create-or-update-character! ec wr data)
		(let* ((object-id (ref data 'object-id)) (character (object-ref wr object-id)))
			(if (not character)
				(let ((character (make-antagonist data)))
					(register-object! wr character)
					(trigger! ec 'creature-create object-id)
					character
				)
				(let ((changes (update-character! character data)))
					(when (not (null? changes))
						(trigger! ec 'creature-update object-id changes)
					)
					character
				)
			)
		)
	)

	; Arguments: connection, event-channel, packet-channel, tick-channel
	(define (handle-interrupt cn ec pc tc)
		(let ((wr (connection-world cn)) (evt (sync/enable-break ec pc (connection-read-thread cn))))
			(cond
				((bytes? evt) ; Incoming packet => handle then wait next.
					(case (get-packet-id evt)
						((#x48) (packet-handler/skill-started cn ec wr (game-server-packet/skill-started evt)))
						(else	(let ((row (hash-ref handlers-table (get-packet-id evt) #f)))
							(if row
								((cdr row) ec wr ((car row) evt))
								(apf-warn "Unhandled packet ~v" evt)
							)
						))
					)

					(handle-interrupt cn ec pc tc)
				)
				((thread? evt) (make-event 'disconnect)) ; Connection closed => return immediately.
				(else ; Postprocess event.
					(apf-debug "Event ~a" evt)
					(case (event-name evt) ; Regular event => postprocess or just return.
						((logout) ; Close socket.
							(disconnect cn)
							evt
						)
						((object-delete) (let ((object-id (second evt)))
							(async-channel-put tc (lambda () ; Discard after event processing.
								(discard-object! wr object-id)
							))
							evt
						))
						((teleport)
							(let ((me (world-me wr))) (when (= (second evt) (object-id me))
								(async-channel-put tc (lambda () ; Discard after event processing.
									(map ; Remove all known objects except myself & party memebers.
										(lambda (object) (discard-object! wr (object-id object)))
										(objects wr (lambda (object)
											(not (or ; TODO Don't remove nearest objects?
												(protagonist? object)
												(member (object-id object) (world-party wr) =)
											))
										))
									)

									(let ((position (ref me 'position)) (angle (ref me 'angle))) ; Request updates.
										(send-packet cn (game-client-packet/validate-location position angle))
										(send-packet cn (game-client-packet/appearing))
									)
								))
							))
							evt
						)
						(else evt)
					)
				)
			)
		)
	)

	(define (packet-handler/user-info ec wr packet)
		(let ((me (world-me wr)))
			(update-protagonist! me packet)
			(register-object! wr me)
			(trigger! ec 'creature-create (ref me 'object-id))
		)
	)
	(define (packet-handler/char-info ec wr packet)
		(let ((antagonist (make-antagonist packet)))
			(register-object! wr antagonist)
			(trigger! ec 'creature-create (ref antagonist 'object-id))
		)
	)
	(define (packet-handler/npc-info ec wr packet)
		(let ((npc (make-npc packet)))
			(register-object! wr npc)
			(trigger! ec 'creature-create (ref npc 'object-id))
		)
	)
	(define (packet-handler/status-update ec wr packet)
		(let* ((creature (object-ref wr (ref packet 'object-id))) (changes (cond
					((protagonist? creature) (update-protagonist! creature packet))
					((antagonist? creature) (update-antagonist! creature packet))
					((npc? creature) (update-npc! creature packet))
			)))
			(when (not (null? changes))
				(trigger! ec 'creature-update (object-id creature) changes)
			)
		)
	)
	(define (packet-handler/spawn-item ec wr packet)
		(let ((item (make-item packet)))
			(register-object! wr item)
			(trigger! ec 'item-create (object-id item))
		)
	)
	(define (packet-handler/object-deleted ec wr packet)
		(let* ((object-id (ref packet 'object-id)) (object (object-ref wr object-id)))
			(when (creature? object) ; Clear targets to deleted object.
				(objects wr (lambda (subject)
					(when (and (creature? subject) (equal? object-id (ref subject 'target-id)))
						(handle-creature-update! ec wr (list (cons 'target-id #f)) subject)
					)
				))
			)
			(trigger! ec 'object-delete object-id)
		)
	)

	(define (packet-handler/move-to-point ec wr packet) ; Происходит, когда существо бежит к точке.
		(handle-creature-update! ec wr packet)
	)
	(define (packet-handler/move-to-pawn ec wr packet) ; Происходит, когда существо бежит к другому существу.
		(let ((target (object-ref wr (ref packet 'target-id))))
			(if target
				(handle-creature-update! ec wr (list
					(cons 'object-id (ref packet 'object-id))
					(cons 'position (ref packet 'position))
					(cons 'destination (ref target 'position))
				))
				(apf-warn "World object ~v is missed, handler: move-to-pawn." (ref packet 'target-id))
			)
		)
	)
	(define (packet-handler/validate-location ec wr packet)
		(handle-creature-update! ec wr packet)
	)
	(define (packet-handler/stop-moving ec wr packet)
		(handle-creature-update! ec wr (list
			(cons 'object-id (ref packet 'object-id))
			(cons 'position (ref packet 'position))
			(cons 'destination #f)
			(cons 'angle (ref packet 'angle))
		))
	)
	(define (packet-handler/change-move-type ec wr packet)
		(let ((creature (object-ref wr (ref packet 'object-id)))) (when creature
			(handle-creature-update! ec wr (list
				(cons 'object-id (object-id creature))
				(cons 'position (get-position creature))
				(cons 'walking? (ref packet 'walking?))
			))
		))
	)
	(define (packet-handler/change-wait-type ec wr packet)
		(handle-creature-update! ec wr (list
			(cons 'object-id (ref packet 'object-id))
			(cons 'position (ref packet 'position))
			(cons 'destination #f)
			(cons 'sitting? (ref packet 'sitting?))
		))
	)

	(define (packet-handler/target-selected ec wr packet)
		(handle-creature-update! ec wr packet)
	)
	(define (packet-handler/target-unselected ec wr packet)
		(handle-creature-update! ec wr (cons (cons 'target-id #f) packet))
	)

	(define (packet-handler/attack ec wr packet)
		(define (handle-hit hit in-combat?)
			(let ((miss? (ref hit 'miss?)))
				(if (not miss?)
					(let ((creature (object-ref wr (ref hit 'target-id))) (shield? (ref hit 'shield?)) (damage (ref hit 'damage)))
						(when creature
							(handle-creature-update! ec wr (list
								(if (and (ref creature 'hp) (not shield?)) (cons 'hp (max 0 (- (ref creature 'hp) damage))) #f)
								(cons 'in-combat? #t)
							) creature)
						)
						#t
					)
					in-combat?
				)
			)
		)

		(let* ((hits (ref packet 'hits)) (in-combat? (fold handle-hit #f hits)))
			(let ((creature (handle-creature-update! ec wr (list ; Fix missing change-moving event & subject in-combat? state.
					(cons 'object-id (ref packet 'object-id))
					(cons 'position (ref packet 'position))
					(cons 'destination #f)
					(if in-combat? (cons 'in-combat? #t) #f)
				))))

				(when creature ; Trigger main event.
					(trigger! ec 'attack
						(object-id creature)
						hits
					)
				)
			)
		)
	)
	(define (packet-handler/social-action ec wr packet)
		(trigger! ec 'gesture
			(ref packet 'object-id)
			(ref packet 'action)
		)
	)

	(define (packet-handler/skill-list ec wr packet)
		(let ((skills (world-skills wr)))
			(hash-clear! skills)
			(apply hash-set*! skills (apply append (map (lambda (i)
				(let ((skill-id (ref i 'skill-id)) (level (ref i 'level)) (active? (ref i 'active?)))
					(list skill-id (make-skill skill-id level active?))
				)
			) (ref packet 'list))))

			(trigger! ec 'skill-list (object-id (world-me wr)))
		)
	)
	(define (packet-handler/skill-started cn ec wr packet)
		(define (make-active-skill packet)
			(make-skill (ref packet 'skill-id) (ref packet 'level) #t (current-milliseconds) (ref packet 'reuse-delay))
		)

		(let* ((skill (make-active-skill packet)) (creature (handle-creature-update! ec wr (list
					(cons 'object-id (ref packet 'object-id))
					(cons 'target-id (ref packet 'target-id))
					(cons 'position (ref packet 'position))
					(cons 'casting skill)
			))))
			(when (protagonist? creature)
				(hash-set! (world-skills wr) (skill-id skill) skill)

				; Fix skill reused event via timer.
				(let ((last-usage (ref skill 'last-usage)) (reuse-delay (ref skill 'reuse-delay)))
					(when (> (or reuse-delay 0) 0)
						(alarm! #:id 'skill-reused cn (+ last-usage reuse-delay)
							(object-id creature) skill
						)
					)
				)
			)

			(when creature
				(trigger! ec 'skill-started (object-id creature) skill)
			)
		)
	)
	(define (packet-handler/skill-launched ec wr packet)
		(let ((creature (object-ref wr (ref packet 'object-id)))) (when creature
			(let ((skill (or (ref creature 'casting) (make-skill (ref packet 'skill-id) (ref packet 'level) #t))))
				(handle-creature-update! ec wr (list
					(cons 'target-id (ref packet 'target-id))
					(cons 'casting #f)
					(cons 'located-at (current-milliseconds))
				) creature)

				(trigger! ec 'skill-launched (object-id creature) skill)
			)
		))
	)
	(define (packet-handler/skill-canceled ec wr packet)
		(let ((creature (object-ref wr (ref packet 'object-id)))) (when creature
			(let ((skill (ref creature 'casting)))
				(update-creature! creature (list
					(cons 'casting #f)
					(cons 'located-at (current-milliseconds))
				))
				(trigger! ec 'skill-canceled (object-id creature) skill)
			)
		))
	)

	(define (packet-handler/ask-join-clan ec wr packet)
		(trigger! ec 'ask/join-clan
			(cons 'clan (ref packet 'name))
		)
	)
	(define (packet-handler/ask-join-party ec wr packet)
		(trigger! ec 'ask/join-party
			(cons 'player (ref packet 'name))
			(cons 'loot (ref packet 'loot))
		)
	)
	(define (packet-handler/ask-be-friends ec wr packet)
		(trigger! ec 'ask/be-friends
			(cons 'player (ref packet 'name))
		)
	)
	(define (packet-handler/ask-join-alliance ec wr packet)
		(trigger! ec 'ask/join-alliance
			(cons 'alliance (ref packet 'name))
		)
	)
	(define (packet-handler/reply-join-party ec wr packet)
		(when (not (ref packet 'accept?))
			(trigger! ec 'reject/join-party
				; (cons 'object-id ?) ; TODO
				; (cons 'name ?) ; TODO?
			)
		)
	)

	(define (packet-handler/party-all-members ec wr packet)
		(set-world-party! wr (apply make-party
			(ref packet 'loot-mode)
			(ref packet 'leader-id)
			(fold (lambda (data members)
				(let ((character (create-or-update-character! ec wr data)))
					(if (not (= (object-id character) (ref packet 'leader-id)))
						(cons (object-id character) members) ; Regular member.
						members ; Party leader.
					)
				)
			) (list) (ref packet 'members))
		))

		(let ((party (world-party wr)))
			(trigger! ec 'party-join (party-loot party) (party-members party))
		)
	)
	(define (packet-handler/party-add-member ec wr packet)
		(let ((character (create-or-update-character! ec wr packet)))
			(trigger! ec 'party-memeber-join (object-id character))
		)
	)
	(define (packet-handler/party-member-update ec wr packet)
		(create-or-update-character! ec wr packet)
	)
	(define (packet-handler/party-member-position ec wr packet)
		(map (lambda (data) (handle-creature-update! ec wr data)) (ref packet 'members))
	)
	(define (packet-handler/party-delete-member ec wr packet)
		(let ((object-id (ref packet 'object-id)))
			(party-kick! wr object-id)
			(trigger! ec 'party-memeber-leave object-id)
		)
	)
	(define (packet-handler/party-clear-members ec wr packet)
		(party-clear! wr)
		(trigger! ec 'party-leave)
	)

	(define (packet-handler/chat-message ec wr packet)
		(trigger! ec 'message
			(ref packet 'object-id)
			(ref packet 'channel)
			(ref packet 'author)
			(ref packet 'text)
		)
	)
	(define (packet-handler/system-message ec wr packet)
		(let ((message-id (ref packet 'message-id)) (arguments (ref packet 'arguments)))
			(when (or (= message-id 612) (= message-id 357)) ; Set spoiled state here bacause there is no other way.
				(let ((target-id (ref (world-me wr) 'target-id)))
					(let ((changes (update-npc! (object-ref wr target-id) (list (cons 'spoiled? #t)))))
						(when (not (null? changes)) (trigger! ec 'creature-update target-id changes))
					)
				)
			)

			; TODO Set is effect failed?

			(apply trigger! ec 'system-message message-id arguments)
		)
	)

	(define (packet-handler/die ec wr packet)
		(let ((creature (object-ref wr (ref packet 'object-id)))) (when creature
			(cond
				((protagonist? creature)
					(update-protagonist! creature (list
						(cons 'hp 0)
						(cons 'died? #t)
						(cons 'alike-dead? #t)
					))
				)
				((npc? creature)
					(update-npc! creature (list
						(cons 'spoiled? (ref packet 'spoiled?))
						(cons 'alike-dead? #t)
					))
				)
				(else
					(update-creature! creature (list
						(cons 'alike-dead? #t)
					))
				)
			)

			; Fix missing change-moving event.
			(handle-creature-update! ec wr (list
				(cons 'object-id object-id)
				(cons 'position (get-position creature))
				(cons 'destination #f)
			))

			; Trigger main event.
			(trigger! ec 'die
				(object-id creature)
				(if (protagonist? creature)
					(ref packet 'return)
					#f
				)
			)
		))
	)
	(define (packet-handler/revive ec wr packet)
		(let ((creature (object-ref wr (ref packet 'object-id)))) (when creature
			(cond
				((protagonist? creature)
					(update-protagonist! creature (list
						(cons 'died? #f)
						(cons 'alike-dead? #f)
					))
				)
				(else
					(update-creature! creature (list
						(cons 'alike-dead? #f)
					))
				)
			)

			(trigger! ec 'revive (object-id creature))
		))
	)

	(define (packet-handler/teleport ec wr packet)
		(let ((creature (handle-creature-update! ec wr (cons (cons 'destination #f) packet))))
			(when creature
				(trigger! ec 'teleport (object-id creature) (ref creature 'position))
			)
		)
	)

	(define (packet-handler/logout ec wr packet)
		(trigger! ec 'logout)
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
		; (cons #x0c (cons game-server-packet/drop-item packet-handler/drop-item)) ; DropItem
		; (cons #x0d (cons game-server-packet/get-item packet-handler/get-item))
		(cons #x0e (cons game-server-packet/status-update packet-handler/status-update))
		; #x10 SellList
		(cons #x12 (cons game-server-packet/object-deleted packet-handler/object-deleted))
		; #x13 CharSelectInfo
		; #x15 CharSelected
		; #x17 CharTemplates
		(cons #x16 (cons game-server-packet/npc-info packet-handler/npc-info))
		; (cons #x1b (cons game-server-packet/item-list packet-handler/item-list))
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
		; (cons #x3a (cons game-server-packet/reply-join-party packet-handler/reply-join-party)) ; reject
		; (cons #x45 (cons game-server-packet/shortcut-init packet-handler/shortcut-init)) ; shortcut-list
		(cons #x47 (cons game-server-packet/stop-moving packet-handler/stop-moving)) ; change-moving
		; #x48 packet-handler/skill-started (especial handling).
		(cons #x49 (cons game-server-packet/skill-canceled packet-handler/skill-canceled)) ; skill-canceled
		(cons #x4a (cons game-server-packet/chat-message packet-handler/chat-message))
		; #x4b EquipUpdate
		; #x4c DoorInfo
		; #x4d DoorStatusUpdate
		(cons #x4e (cons game-server-packet/party-all-members packet-handler/party-all-members)) ; party-join
		(cons #x4f (cons game-server-packet/party-add-member packet-handler/party-add-member)) ; party-memeber-join
		(cons #x50 (cons game-server-packet/party-clear-members packet-handler/party-clear-members)) ; party-leave
		(cons #x51 (cons game-server-packet/party-delete-member packet-handler/party-delete-member)) ; party-memeber-leave
		(cons #x52 (cons game-server-packet/party-member-update packet-handler/party-member-update)) ; creature-update
		(cons #x58 (cons game-server-packet/skill-list packet-handler/skill-list))
		(cons #x60 (cons game-server-packet/move-to-pawn packet-handler/move-to-pawn)) ; change-moving
		(cons #x61 (cons game-server-packet/validate-location packet-handler/validate-location))
		(cons #x64 (cons game-server-packet/system-message packet-handler/system-message))
		; #x65 StartPledgeWar
		; #x6d SetupGauge
		; #x6f ChooseInventoryItem
		(cons #x76 (cons game-server-packet/skill-launched packet-handler/skill-launched)) ; skill-launched ; TODO 8e?
		; #x76 SetToLocation
		(cons #x7d (cons game-server-packet/ask-be-friends packet-handler/ask-be-friends)) ; ask
		(cons #x7e (cons void packet-handler/logout)) ; logout
		; (cons #x7f (cons game-server-packet/ packet-handler/)) ; MagicEffectIcons
		; (cons #x80 (cons game-server-packet/quest-list packet-handler/quest-list)) ; quest-list
		; #x81 EnchantResult
		; #x86 Ride
		; #x98 PlaySound
		; #x99 StaticObject
		; #xa6 MyTargetSelected
		(cons #xa7 (cons game-server-packet/party-member-position packet-handler/party-member-position)) ; creature-update
		(cons #xa8 (cons game-server-packet/ask-join-alliance packet-handler/ask-join-alliance)) ; ask
		; #xc4 Earthquake
		; #xc8 NormalCamera
		; #xd0 MultiSellList
		; #xd4 Dice
		; #xd5 Snoop
		; (cons #xe4 'henna-info (cons game-server-packet/henna-info packet-handler/henna-info))
		; (cons #xe7 'macro-list (cons game-server-packet/macro-list packet-handler/macro-list))
		; #xee PartySpelled
		; #xf5 SSQStatus
		; (cons #xf8 'signs-sky (cons game-server-packet/signs-sky packet-handler/signs-sky)) SignsSky
		; (cons #xfe 'Ex* (cons game-server-packet/ packet-handler/))

		; (cons #x?? game-server-packet/ packet-handler/)
	)))
)
