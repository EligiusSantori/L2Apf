(module system racket/base
	(require
		srfi/1
		racket/dict
		racket/function
		"../library/structure.scm"
		"../_logic.scm"
		"../model/world.scm"
		"../model/object.scm"
		"../model/creature.scm"
		"../model/npc.scm"
		"../model/character.scm"
		"../model/antagonist.scm"
		"../model/protagonist.scm"
		;"../model/quest.scm"
		"../model/skill.scm"
		;"../model/item.scm"
		"../packet/game/server/chat_message.scm"
		"../packet/game/server/system_message.scm"
		"../packet/game/server/social_action.scm"
		"../packet/game/server/attack.scm"
		"../packet/game/server/die.scm"
		"../packet/game/server/revive.scm"
		"../packet/game/server/object_deleted.scm"
		"../packet/game/server/user_info.scm"
		"../packet/game/server/char_info.scm"
		"../packet/game/server/npc_info.scm"
		"../packet/game/server/move_to_point.scm"
		"../packet/game/server/move_to_pawn.scm"
		"../packet/game/server/stop_moving.scm"
		"../packet/game/server/status_update.scm"
		"../packet/game/server/target_selected.scm"
		"../packet/game/server/target_unselected.scm"
		"../packet/game/server/ask_join_alliance.scm"
		"../packet/game/server/ask_join_clan.scm"
		"../packet/game/server/ask_join_party.scm"
		"../packet/game/server/ask_be_friends.scm"
		"../packet/game/server/change_move_type.scm"
		"../packet/game/server/change_wait_type.scm"
		"../packet/game/server/skill_list.scm"
		"../packet/game/server/skill_started.scm"
		"../packet/game/server/skill_launched.scm"
		"../packet/game/server/skill_canceled.scm"
	)
	
	(provide packet-handlers-table)

	; handlers
	
	(define (packet-handler/move-to-point world packet)
		(let ((object-id (@: packet 'object-id)) (p (@: packet 'position)) (d (@: packet 'destination)))
			(let ((moving? (not (equal? p d))) (creature (object-ref world object-id)))
				(define struct (list
					(cons 'position p)
					(cons 'destination d)
					(cons 'moving? moving?)
				))
				(when creature
					(update-creature! creature (if moving? (alist-cons 'angle (points-angle p d) struct) struct))
				)
				(values object-id p (if moving? d #f))
			)
		)
	)

	(define (packet-handler/char-info world packet)
		(let ((antagonist (create-antagonist packet)))
			(register-object! world antagonist)
			(values (@: antagonist 'object-id))
		)
	)

	(define (packet-handler/user-info world packet)
		(let ((me (@: world 'me)))
			(update-protagonist! me packet)
			(register-object! world me)
			(values (@: me 'object-id))
		)
	)
	
	(define (packet-handler/attack world packet)
		(let* ((object-id (@: packet 'object-id)) (creature (object-ref world object-id)) (hits (@: packet 'hits)))
			(when creature
				(update-creature! creature (list
					(cons 'target-id (@: (car hits) 'target-id))
					(cons 'position (@: packet 'position))
					(cons 'destination (@: packet 'position))
					(cons 'moving? #f)
				))
				; TODO
					; ? substract damage from hp
					; ? refresh in-combat for self and all targets if not miss
			)
			(values object-id hits)
		)
	)

	(define (packet-handler/die world packet)
		(let* ((object-id (@: packet 'object-id)) (creature (object-ref world object-id)) (spoiled? (@: packet 'spoiled?)))
			(let ((data (list (cons 'moving? #f) (cons 'alike-dead? #t))))
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

	(define (packet-handler/status-update world packet)
		(let* ((object-id (@: packet 'object-id)) (creature (object-ref world object-id)))
			(cond
				((protagonist? creature) (update-protagonist! creature packet))
				((antagonist? creature) (update-antagonist! creature packet))
				((npc? creature) (update-npc! creature packet))
			)
			(values object-id)
		)
	)

	(define (packet-handler/object-deleted world packet)
		(let ((object-id (@: packet 'object-id)))
			(discard-object! world object-id)
			(values object-id) ; TODO return object-id but discard after event handle
		)
	)

	(define (packet-handler/npc-info world packet)
		(let ((npc (create-npc packet)))
			(register-object! world npc)
			(values (@: npc 'object-id))
		)
	)

	(define (packet-handler/target-selected world packet)
		(let* ((object-id (@: packet 'object-id)) (target-id (@: packet 'target-id)) (creature (object-ref world object-id)))
			(when creature
				(update-creature! creature (list
					(cons 'target-id target-id)
					(cons 'position (@: packet 'position))
				))
			)
			(values object-id target-id)
		)
	)

	(define (packet-handler/target-unselected world packet)
		(let* ((object-id (@: packet 'object-id)) (creature (object-ref world object-id)))
			(when creature
				(update-creature! creature (list
					(cons 'target-id #f)
					(cons 'position (@: packet 'position))
				))
			)
			(values object-id #f)
		)
	)

	(define (packet-handler/social-action world packet)
		(values (@: packet 'object-id) (@: packet 'action))
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

	(define (packet-handler/stop-moving world packet)
		(let* ((object-id (@: packet 'object-id)) (p (@: packet 'position)) (a (@: packet 'angle)) (creature (object-ref world object-id)))
			(when creature
				(update-creature! creature (list
					(cons 'angle a)
					(cons 'position p)
					(cons 'destination p)
					(cons 'moving? #f)
				))
			)
			(values object-id p #f)
		)
	)

	(define (packet-handler/chat-message world packet)
		(values
			(@: packet 'object-id)
			(@: packet 'channel)
			(@: packet 'author)
			(@: packet 'text)
		)
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
	
	(define (packet-handler/skill-list world packet)
		(define (f i)
			(let ((skill-id (@: i 'skill-id)) (level (@: i 'level)) (active? (@: i 'active?)))
				(list skill-id (make-skill skill-id level active?))
			)
		)
	
		(let ((skills (@: world 'skills)) (l (@: packet 'list)))
			(hash-clear! skills)
			(apply hash-set*! skills (apply append (map f l)))
			(values (hash-values skills))
		)
	)
	
	(define (packet-handler/move-to-pawn world packet) ; Происходит, когда игрок бежит не к точке, а к монстру или NPC
		(let* ((object-id (@: packet 'object-id)) (target-id (@: packet 'target-id)) (position (@: packet 'position)))
			(let* ((creature (object-ref world object-id)) (target (object-ref world target-id)))
				(if (and creature target)
					(let* ((destination (@: target 'position)) (moving? (not (equal? position destination))))
						(define struct (list
							(cons 'position position)
							(cons 'destination destination)
							(cons 'moving? moving?)
						))
						(update-creature! creature (if moving? (alist-cons 'angle (points-angle position destination) struct) struct))
						(values object-id position (if moving? destination #f))
					)
					(values object-id position #f) ; TODO omit event
				)
			)
		)
	)
	
	(define (packet-handler/skill-started world packet)
		(let* ((object-id (@: packet 'object-id)) (skill-id (@: packet 'skill-id)) (level (@: packet 'level)) (creature (object-ref world object-id)))
			(when creature
				(update-creature! creature (list
					(cons 'target-id (@: packet 'target-id))
					(cons 'position (@: packet 'position))
					(cons 'casting? #t)
				))
			)
			
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
	
	(define (packet-handler/system-message world packet)
		(let ((message-id (@: packet 'message-id)) (arguments (@: packet 'arguments)))
			
			; set spoiled state here bacause no other way
			(when (or (= message-id 612) (= message-id 357))
				(let ((target-id (@: world 'me 'target-id)))
					(update-npc! (object-ref world target-id) (list
						(cons 'spoiled? #t)
					))
				)
			)
			
			(values message-id arguments)
		)
	)

	; table
	
	(define (applicate row)
		(let-values (((id name packet handler) (apply values row)))
			(list id name (lambda (world buffer) (handler world (packet buffer))))
		)
	)
	
	(define packet-handlers-table (make-hash (map applicate (list ; id, name, handler(world, buffer)
		(list #x01 'change-moving game-server-packet/move-to-point packet-handler/move-to-point)
		(list #x03 'creature-create game-server-packet/char-info packet-handler/char-info)
		(list #x04 'creature-update game-server-packet/user-info packet-handler/user-info)
		(list #x05 'attack game-server-packet/attack packet-handler/attack)
		(list #x06 'die game-server-packet/die packet-handler/die)
		(list #x07 'revive game-server-packet/revive packet-handler/revive)
		; #x0a AttackCanceld
		;(list #x0b ' game-server-packet/spawn-item packet-handler/spawn-item) SpawnItem | SpawnItemPoly
		;(list #x0c ' game-server-packet/drop-item packet-handler/drop-item) DropItem
		;(list #x0d ' game-server-packet/get-item packet-handler/get-item)
		(list #x0e 'creature-update game-server-packet/status-update packet-handler/status-update)
		; #x10 SellList
		(list #x12 'object-delete game-server-packet/object-deleted packet-handler/object-deleted)
		; #x13 CharSelectInfo
		; #x15 CharSelected
		; #x17 CharTemplates
		(list #x16 'creature-create game-server-packet/npc-info packet-handler/npc-info)
		;(list #x1b ' game-server-packet/item-list packet-handler/item-list)
		; #x25 ActionFailed
		(list #x29 'change-target game-server-packet/target-selected packet-handler/target-selected)
		(list #x2a 'change-target game-server-packet/target-unselected packet-handler/target-unselected)
		; #x2b ? AutoAttackStart
		; #x2c ? AutoAttackStop
		(list #x2d 'gesture game-server-packet/social-action packet-handler/social-action)
		(list #x2e 'creature-update game-server-packet/change-move-type packet-handler/change-move-type)
		(list #x2f 'creature-update game-server-packet/change-wait-type packet-handler/change-wait-type)
		(list #x32 'ask game-server-packet/ask-join-clan packet-handler/ask-join-clan)
		(list #x39 'ask game-server-packet/ask-join-party packet-handler/ask-join-party)
		;(list #x45 'shortcut-list game-server-packet/shortcut-init packet-handler/shortcut-init)
		(list #x47 'change-moving game-server-packet/stop-moving packet-handler/stop-moving)
		(list #x48 'skill-started game-server-packet/skill-started packet-handler/skill-started)
		(list #x49 'skill-canceled game-server-packet/skill-canceled packet-handler/skill-canceled)
		(list #x4a 'message game-server-packet/chat-message packet-handler/chat-message)
		; #x4b EquipUpdate
		; #x4c DoorInfo
		; #x4d DoorStatusUpdate
		; #x4e ?
		; #x52 ?
		(list #x58 'skill-list game-server-packet/skill-list packet-handler/skill-list)
		(list #x60 'change-moving game-server-packet/move-to-pawn packet-handler/move-to-pawn)
		(list #x64 'system-message game-server-packet/system-message packet-handler/system-message)
		; #x65 StartPledgeWar
		; #x6d SetupGauge
		; #x6f ChooseInventoryItem
		(list #x76 'skill-launched game-server-packet/skill-launched packet-handler/skill-launched)
		; #x76 SetToLocation
		(list #x7d 'ask game-server-packet/ask-be-friends packet-handler/ask-be-friends)
		(list #x7e 'logout void (const (list)))
		;(list #x7f ' game-server-packet/ packet-handler/) MagicEffectIcons
		;(list #x80 'quest-list game-server-packet/quest-list packet-handler/quest-list)
		; #x81 EnchantResult
		; #x86 Ride
		; #x98 PlaySound
		; #x99 StaticObject
		; #xa6 MyTargetSelected
		;(list #xa7 ' game-server-packet/ packet-handler/)
		(list #xa8 'ask game-server-packet/ask-join-alliance packet-handler/ask-join-alliance)
		; #xc4 Earthquake
		; #xc8 NormalCamera
		; #xd0 MultiSellList
		; #xd4 Dice
		; #xd5 Snoop
		;(list #xe4 'henna-info game-server-packet/henna-info packet-handler/henna-info)
		;(list #xe7 'macro-list game-server-packet/macro-list packet-handler/macro-list)
		; #xee PartySpelled
		; #xf5 SSQStatus
		;(list #xf8 'signs-sky game-server-packet/signs-sky packet-handler/signs-sky) SignsSky
		;(list #xfe 'Ex* game-server-packet/ packet-handler/)
		
		;(list # ' game-server-packet/ packet-handler/)
	))))
)