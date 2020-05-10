(module system racket/base ; Intermediate layer between low-level protocol and user API to provide higher-level events and state of game world.
	(require
		(only-in srfi/1 fold car+cdr)
		racket/function
		racket/set
		racket/async-channel
		racket/contract
		"../library/extension.scm"
		"../library/date_time.scm"
		"../library/geometry.scm"
		"error.scm"
		"log.scm"
		"structure.scm"
		"connection.scm"
		"event.scm"
		(only-in "../packet/packet.scm" get-packet-id)
		(relative-in "../packet/game/server/."
			"user_info.scm"
			"char_info.scm"
			"npc_info.scm"
			"status_update.scm"
			"object_deleted.scm"
			"validate_location.scm"
			"move_to_point.scm"
			"move_to_pawn.scm"
			"stop_moving.scm"
			"change_move_type.scm"
			"change_wait_type.scm"
			"target_selected.scm"
			"target_unselected.scm"
			"my_target_selected.scm"
			"social_action.scm"
			"attack.scm"
			"skill_list.scm"
			"skill_started.scm"
			"skill_launched.scm"
			"skill_canceled.scm"
			"die.scm"
			"revive.scm"
			"teleport.scm"
			"inventory_info.scm"
			"inventory_update.scm"
			"spawn_item.scm"
			"drop_item.scm"
			"pick_item.scm"
			"chat_message.scm"
			"system_message.scm"
			"ask_join_alliance.scm"
			"ask_join_clan.scm"
			"ask_join_party.scm"
			"ask_be_friends.scm"
			"reply_join_party.scm"
			"party_all_members.scm"
			"party_add_member.scm"
			"party_member_update.scm"
			"party_member_position.scm"
			"party_delete_member.scm"
			"party_clear_members.scm"
		)
		(relative-in "../packet/game/client/."
			"validate_location.scm"
			"refresh_skill_list.scm"
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

	(struct exn:error:handler exn:fail:user (name)
		#:extra-constructor-name make-exn:error:handler
		#:transparent
	)
	(define (raise-handler-error packet-name message . args)
		(raise (make-exn:error:handler (error-format message args) (current-continuation-marks) packet-name))
	)
	(define (trigger ec name . data) (async-channel-put ec (apply make-event (cons name data))) name); async-channel? symbol? any/c ...
	; (define (trigger-change-moving! cn object-id from to) (trigger cn 'change-moving object-id from to)) ; connection? point? point?
	; (define (trigger-creature-create! cn creature) (trigger cn 'creature-create creature)) ; connection? creature?
	(define (trigger-creature-update! ec object-id changes)
		(let ((fc (alist-except changes eq? 'target-id 'position 'destination 'angle 'walking? 'sitting?)))
			(when (not (null? fc))
				(trigger ec 'creature-update object-id fc)
			)
		)
	)
	(define delayed-stop-moving (gensym))
	(define (will-finish-move creature)
		(let ((destination (ref creature 'destination)) (speed (get-move-speed creature)))
			(when (zero? speed) (apf-warn "Creature ~v started move but speed is not known." creature))
			(and destination (not (null? speed)) ; Can be 0 if no walk-speed or run-speed known.
				(+ (ref creature 'located-at) (/ (distance/3d (ref creature 'position) destination) (get-move-speed creature)))
			)
		)
	)
	(define (delayed-skill-reused! cn skill) ; Fix skill reused event via timer.
		(let ((last-usage (ref skill 'last-usage))
				(reuse-delay (ref skill 'reuse-delay))
				(ev (make-event 'skill-reused skill)))
			(and reuse-delay (> reuse-delay 0)
				(if (> (+ last-usage reuse-delay) (timestamp))
					(alarm! cn (+ last-usage reuse-delay) ev)
					(trigger! cn ev)
				)
			)
		)
	)

	(define (handle-creature-update! ec wr data [creature #f])
		(let* ((data (filter identity data)) (creature (or creature (object-ref wr (ref data 'object-id)))))
			(when creature
				(let ((changes (update-creature! creature data)))
					(let ((change (assoc 'target-id changes eq?))) (when change
						(trigger ec 'change-target (object-id creature) (cadr change) (cddr change))
					))
					(let ((pc (assoc 'position changes eq?)) (dc (assoc 'destination changes eq?)))
						(when dc ; If start or stop moving then trigger event (skip position validation).
							(trigger ec 'change-moving (object-id creature) (ref creature 'position) (cadr dc))
						)
						(when (or pc dc) ; If position has been updated.
							(trigger ec delayed-stop-moving
								(object-id creature)
								(will-finish-move creature)
								(ref creature 'destination)
								(ref creature 'moving-timer)
							)
						)
					)
					(let ((change (assoc 'walking? changes eq?))) (when change
						(trigger ec 'change-walking (object-id creature) (cadr change))
					))
					(let ((change (assoc 'sitting? changes eq?))) (when change
						(trigger ec 'change-sitting (object-id creature) (cadr change))
					))
					(trigger-creature-update! ec (object-id creature) changes)
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
					(trigger ec 'creature-create object-id)
					character
				)
				(let ((changes (update-character! character data)))
					(trigger-creature-update! ec object-id changes)
					character
				)
			)
		)
	)

	; Arguments: connection, event-channel, packet-channel, tick-channel
	(define (handle-interrupt cn ec pc tc)
		(let ((wr (connection-world cn)) (ev (sync/enable-break ec pc (connection-read-thread cn))))
			(cond
				((bytes? ev) ; Incoming packet => handle then wait next.
					(if (= (get-packet-id ev) #x64)
						(packet-handler/system-message cn ec wr (game-server-packet/system-message ev)) ; Special case.
						(let ((row (hash-ref handlers-table (get-packet-id ev) #f)))
							(if row
								((cdr row) ec wr ((car row) ev))
								(apf-warn "Unhandled packet ~v" ev)
							)
						)
					)

					(handle-interrupt cn ec pc tc)
				)
				((thread? ev) (make-event 'disconnect)) ; Connection closed => return immediately.
				(else ; Preprocess or skip regular event.
					(let ((ev (or (preprocess-event cn wr ev) (handle-interrupt cn ec pc tc))))
						(apf-debug "Event ~a" ev)
						ev
					)
				)
			)
		)
	)

	(define (preprocess-event cn wr ev)
		(case-event ev
			('logout () ; Close socket.
				(disconnect cn)
				ev
			)
			('creature-create (subject-id)
				(when (= subject-id (object-id (world-me wr)))
					(send-packet cn (game-client-packet/refresh-skill-list))
				)
				ev
			)
			('object-delete (subject-id)
				(let ((creature (object-ref wr subject-id)))
					(when (creature? creature) (timer-stop! cn (ref creature 'moving-timer)))
				)

				(next-tick! cn (lambda () ; Discard after event processing.
					(attackers-delete! (world-me wr) subject-id)
					(discard-object! wr subject-id)
				))
				ev
			)
			('teleport (subject-id destination)
				(let ((me (world-me wr))) (when (= subject-id (object-id me))
					(let ((position (ref me 'position)) (angle (ref me 'angle))) ; Request updates.
						(send-packet cn (game-client-packet/validate-location position angle))
						(send-packet cn (game-client-packet/appearing))
					)
					(attackers-clear! me)
				))
				ev
			)
			('change-target (subject-id target-id . rest)
				(let ((me (world-me wr)) (subject (object-ref wr subject-id)))
					(if (eq? (object-id me) target-id)
						(when (npc? subject)
							(attackers-add! me subject-id)
						)
						(when (attackers-has? me subject-id)
							(next-tick! cn (lambda () (attackers-delete! me subject-id)))
						)
					)
				)
				ev
			)
			('attack (subject-id hits)
				(let ((me (world-me wr)))
					(when (assoc (object-id me) hits =)
						(attackers-add! me subject-id)
					)
				)
				ev
			)
			('skill-started (subject-id target-id skill)
				(let ((me (world-me wr))) (cond
					((= (object-id me) subject-id) ; I'm caster.
						(delayed-skill-reused! cn skill)
					)
					((eq? (object-id me) target-id) ; I'm target.
						(when (skill-harmful? (skill-id skill))
							(attackers-add! me subject-id)
						)
					)
					((and (attackers-has? me subject-id) (not (eq? target-id subject-id)))
						(next-tick! cn (lambda () (attackers-delete! me subject-id)))
					)
				))
				ev
			)
			('skill-launched (subject-id skill target-ids)
				(let ((me (world-me wr)))
					(when (and (member (object-id me) target-ids =) (skill-harmful? (skill-id skill)))
						(attackers-add! me subject-id)
					)
				)
				ev
			)
			('die (subject-id . rest)
				(let ((me (world-me wr)))
					(when (attackers-has? me subject-id)
						(next-tick! cn (lambda () (attackers-delete! me subject-id)))
					)
				)
				ev
			)
			('change-moving (subject-id position destination)
				(when (not destination)
					(let ((subject (object-ref wr subject-id)))
						(when (and (creature? subject) (ref subject 'destination)) ; Stop moving by timer.
							(update-creature! subject (list
								(cons 'position position)
								(cons 'destination destination)
							))
						)
					)
				)
				ev
			)
			(delayed-stop-moving (subject-id will-finish destination timer)
				(timer-stop! cn timer) ; If not moving just reset stop-moving event timer.
				(when will-finish
					(let ((ev (make-event 'change-moving subject-id destination #f)))
						(if (> will-finish (timestamp))
							(alarm! #:id timer cn will-finish ev)
							(trigger! cn ev)
						)
					)
				)
				#f
			)
			(else ev)
		)
	)

	(define (packet-handler/user-info ec wr packet)
		(let* ((me (world-me wr)) (changes (update-protagonist! me packet)))
			(if (not (object-ref wr (ref packet 'object-id)))
				(begin
					(register-object! wr me)
					(trigger ec 'creature-create (object-id me))
				)
				(trigger-creature-update! ec (object-id me) changes)
			)
		)
	)
	(define (packet-handler/char-info ec wr packet)
		(create-or-update-character! ec wr packet)
	)
	(define (packet-handler/npc-info ec wr packet)
		(let* ((object-id (ref packet 'object-id)) (npc (object-ref wr object-id)))
			(if (not npc)
				(let ((npc (make-npc packet)))
					(register-object! wr npc)
					(trigger ec 'creature-create object-id)
				)
				(let ((changes (update-npc! npc packet)))
					(trigger-creature-update! ec object-id changes)
				)
			)
		)
	)
	(define (packet-handler/status-update ec wr packet)
		(let* ((creature (object-ref wr (ref packet 'object-id))) (changes (cond
					((protagonist? creature) (update-protagonist! creature packet))
					((antagonist? creature) (update-antagonist! creature packet))
					((npc? creature) (update-npc! creature packet))
			)))
			(when (not (null? changes))
				(trigger ec 'creature-update (object-id creature) changes)
			)
		)
	)
	(define (packet-handler/inventory-info ec wr packet)
		(let ((inv (world-inventory wr)))
			(hash-clear! inv)
			(map (lambda (item)
				(hash-set! inv (ref item 'object-id) (make-item item))
			) (ref packet 'items))
		)
	)
	(define (packet-handler/inventory-update ec wr packet)
		(let ((inv (world-inventory wr)))
			(map (lambda (item)
				(case (ref item 'change)
					((add) (hash-set! inv (ref item 'object-id) (make-item item)))
					((modify) (hash-set! inv (ref item 'object-id) (make-item item)))
					((remove) (hash-remove! inv (ref item 'object-id)))
					(else (void))
				)
			) (ref packet 'items))
		)
	)
	(define (packet-handler/spawn-item ec wr packet)
		(let ((item (make-item packet)))
			(register-object! wr item)
			(trigger ec 'item-spawn (object-id item) #f)
		)
	)
	(define (packet-handler/drop-item ec wr packet)
		(let ((item (make-item packet)))
			(register-object! wr item)
			(trigger ec 'item-spawn (object-id item) (ref packet 'subject-id))
		)
	)
	(define (packet-handler/pick-item ec wr packet) ; Происходит перед object-delete.
		(trigger ec 'item-pick (ref packet 'object-id) (ref packet 'subject-id) (ref packet 'position))
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
			(trigger ec 'object-delete object-id)
		)
	)

	(define (packet-handler/move-to-point ec wr packet) ; Происходит, когда существо бежит к точке.
		(handle-creature-update! ec wr packet)
	)
	(define (packet-handler/move-to-pawn ec wr packet) ; Происходит, когда существо бежит к другому существу или когда начинается каст (возможно атака тоже).
		(let ((creature (object-ref wr (ref packet 'object-id))) (target (object-ref wr (ref packet 'target-id))))
; (when (antagonist? creature) (printf "<~a> {move-to-pawn ~a}~n" (format-time) packet)(flush-output))
			(if (and creature target)
				(handle-creature-update! ec wr (if (npc? creature)
					(begin
						(list
							(cons 'object-id (object-id creature))
							(cons 'target-id (object-id target))
							(cons 'position (ref packet 'position))
							(cons 'destination (ref target 'position))
						)
					)
					(list
						(cons 'object-id (object-id creature))
						(cons 'position (ref packet 'position))
					)
				) creature)
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
	(define (packet-handler/my-target-selected ec wr packet)
		(handle-creature-update! ec wr (list
			(cons 'object-id (object-id (world-me wr)))
			(cons 'target-id (ref packet 'target-id))
		))
	)

	(define (packet-handler/attack ec wr packet)
		(define (group-hits hits)
			(hash->list (fold (lambda (hit map)
				(let* ((target-id (ref hit 'target-id)) (bucket (hash-ref map target-id (list))))
					(hash-set! map target-id (cons (alist-except hit eq? 'target-id) bucket))
					map
				)
			) (make-hasheq) (ref packet 'hits)))
		)
		(define (hit-damage hit)
			(if (not (ref hit 'miss?)) (ref hit 'damage) 0)
		)
		(define (handle-taget subject bucket in-combat?)
			(let-values (((target-id hits) (car+cdr bucket)))
				(let ((victim (object-ref wr target-id))) (when victim
					(let ((damage (apply + (map hit-damage hits))) (hp (ref victim 'hp)))
						(if (> damage 0)
							(begin
								(handle-creature-update! ec wr (list
									(if hp (cons 'hp (max 0 (- hp damage))) #f)
									(cons 'in-combat? #t)
								) victim)
								#t ; Subject in combat now.
							)
							in-combat?
						)
					)
				))
			)
		)

		(let ((subject (object-ref wr (ref packet 'object-id))) (hits (group-hits (ref packet 'hits)))) (when subject
			(let ((in-combat? (fold (bind-head handle-taget subject) #f hits)))
				(handle-creature-update! ec wr (list ; Fix missing change-moving event & subject in-combat? state.
					(cons 'object-id (ref packet 'object-id))
					(cons 'position (ref packet 'position))
					(cons 'destination #f)
					(if in-combat? (cons 'in-combat? #t) #f)
				) subject)

				(trigger ec 'attack (object-id subject) hits) ; Trigger main event.
			)
		))
	)
	(define (packet-handler/social-action ec wr packet)
		(trigger ec 'gesture
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
		)
	)
	(define (packet-handler/skill-started ec wr packet)
		(define (update-or-create-skill packet)
			(or
				(and (= (object-id (world-me wr)) (ref packet 'object-id))
					(let ((skill (skill-ref wr (ref packet 'skill-id)))) (and skill (begin
						(update-skill! skill (list
							(cons 'level (ref packet 'level))
							(cons 'last-usage (timestamp))
							(cons 'reuse-delay (ref packet 'reuse-delay))
						))
						skill
					)))
				)
				(make-skill (ref packet 'skill-id) (ref packet 'level) #t (timestamp) (ref packet 'reuse-delay))
			)
		)

		(let ((creature (object-ref wr (ref packet 'object-id)))) (when creature
			(let ((skill (update-or-create-skill packet)) (me (world-me wr)))
				(handle-creature-update! ec wr (list
					(cons 'object-id (object-id creature))
					; (cons 'target-id (ref packet 'target-id)) ; It's possible to cast on self without target changing.
					(cons 'position (ref packet 'position))
					(cons 'destination #f)
					(cons 'casting skill)
				) creature)

				(trigger ec 'skill-started (object-id creature) (ref packet 'target-id) skill)
			)
		))
	)
	(define (packet-handler/skill-launched ec wr packet)
		(define (find-or-create-skill creature packet)
			(or
				(ref creature 'casting)
				(and (protagonist? creature)
					(skill-ref wr (ref packet 'skill-id))
				)
				(make-skill (ref packet 'skill-id) (ref packet 'level) #t)
			)
		)

		(let ((creature (object-ref wr (ref packet 'object-id))) (me (world-me wr))) (when creature
			(let ((skill (find-or-create-skill creature packet)))
				(handle-creature-update! ec wr (list
					(cons 'casting #f)
					(cons 'located-at (timestamp)) ; FIXME NO REASON, VALUE WILL BE DROPPED.
				) creature)

				(trigger ec 'skill-launched (object-id creature) skill (ref packet 'targets))
			)
		))
	)
	(define (packet-handler/skill-canceled ec wr packet)
		(let ((creature (object-ref wr (ref packet 'object-id)))) (when creature
			(let ((skill (ref creature 'casting)))
				(update-creature! creature (list
					(cons 'casting #f)
					(cons 'located-at (timestamp)) ; FIXME NO REASON, VALUE WILL BE DROPPED.
				))
				(trigger ec 'skill-canceled (object-id creature) skill)
			)
		))
	)

	(define (packet-handler/ask-join-clan ec wr packet)
		(trigger ec 'ask/join-clan
			(cons 'clan (ref packet 'name))
		)
	)
	(define (packet-handler/ask-join-party ec wr packet)
		(trigger ec 'ask/join-party
			(cons 'player (ref packet 'name))
			(cons 'loot (ref packet 'loot))
		)
	)
	(define (packet-handler/ask-be-friends ec wr packet)
		(trigger ec 'ask/be-friends
			(cons 'player (ref packet 'name))
		)
	)
	(define (packet-handler/ask-join-alliance ec wr packet)
		(trigger ec 'ask/join-alliance
			(cons 'alliance (ref packet 'name))
		)
	)
	(define (packet-handler/reply-join-party ec wr packet)
		(when (not (ref packet 'accept?))
			(trigger ec 'reject/join-party
				; (cons 'object-id ?) ; TODO
				; (cons 'name ?) ; TODO?
			)
		)
	)

	(define (packet-handler/party-all-members ec wr packet)
		(let ((me (world-me wr)) (leader-id (ref packet 'leader-id)))
			(set-world-party! wr (apply make-party
				(ref packet 'loot-mode)
				leader-id
				(fold (lambda (data members)
					(let ((character (create-or-update-character! ec wr data)))
						(if (not (member (object-id character) (list (object-id me) leader-id)))
							(cons (object-id character) members) ; Regular member.
							members ; Party leader.
						)
					)
				) (list (object-id me)) (ref packet 'members))
			))
		)

		(let ((party (world-party wr)))
			(trigger ec 'party-join (party-loot party) (party-members party))
		)
	)
	(define (packet-handler/party-add-member ec wr packet)
		(let ((character (create-or-update-character! ec wr packet)))
			(when (not (in-party? (world-party wr) (object-id character)))
				(party-add! wr (object-id character))
				(trigger ec 'party-memeber-join (object-id character))
			)
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
			(if (world-party wr)
				(trigger ec 'party-memeber-leave object-id)
				(trigger ec 'party-leave)
			)
			(trigger ec 'party-memeber-leave object-id)
		)
	)
	(define (packet-handler/party-clear-members ec wr packet)
		(when (world-party wr)
			(party-clear! wr)
			(trigger ec 'party-leave)
		)
	)

	(define (packet-handler/chat-message ec wr packet)
		(trigger ec 'message
			(ref packet 'object-id)
			(ref packet 'channel)
			(ref packet 'author)
			(ref packet 'text)
		)
	)
	(define (packet-handler/system-message cn ec wr packet)
		(let ((message-id (ref packet 'message-id)) (arguments (ref packet 'arguments)))
			(cond
				((or (= message-id 612) (= message-id 357)) ; Set spoiled state here bacause there is no other way.
					(let ((target-id (ref (world-me wr) 'target-id)))
						(let ((changes (update-npc! (object-ref wr target-id) (list (cons 'spoiled? #t)))))
							(when (not (null? changes)) (trigger ec 'creature-update target-id changes))
						)
					)
				)
				((= message-id 48) (let ((skill (skill-ref wr (ref arguments 'skill)))) (when skill
					(when (skill-ready? skill) ; Fix broken skill-ready?
						(let ((reuse-delay (if (> (ref skill 'reuse-delay) 0) (ref skill 'reuse-delay) 3)))
							(set-skill-used! skill (- (+ (timestamp) 3) reuse-delay) reuse-delay) ; Will ready after 3s.
							(delayed-skill-reused! cn skill)
						)
					)
					(trigger ec 'skill-reusing skill)
				)))
			)

			; TODO Set is effect failed?

			(apply trigger ec 'system-message message-id arguments)
		)
	)

	(define (packet-handler/die ec wr packet)
		(let ((creature (object-ref wr (ref packet 'object-id)))) (when creature
			(cond
				((protagonist? creature)
					(update-protagonist! creature (list
						(cons 'hp 0)
						(cons 'dead? #t)
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
				(cons 'object-id (object-id creature))
				(cons 'position (get-position creature))
				(cons 'destination #f)
			))

			; Trigger main event.
			(trigger ec 'die (object-id creature) (if (protagonist? creature) (ref packet 'return) #f))
		))
	)
	(define (packet-handler/revive ec wr packet)
		(let ((creature (object-ref wr (ref packet 'object-id)))) (when creature
			(cond
				((protagonist? creature)
					(update-protagonist! creature (list
						(cons 'dead? #f)
						(cons 'alike-dead? #f)
					))
				)
				(else
					(update-creature! creature (list
						(cons 'alike-dead? #f)
					))
				)
			)

			(trigger ec 'revive (object-id creature))
		))
	)

	(define (packet-handler/teleport ec wr packet)
		(let ((creature (handle-creature-update! ec wr (cons (cons 'destination #f) packet)))) (when creature
			(trigger ec 'teleport (object-id creature) (ref creature 'position))

			(let ((me (world-me wr)) (party (party-members (world-party wr))))
				(when (= (object-id creature) (object-id me))
					(map ; Remove all known objects except myself & party memebers.
						(lambda (object) (trigger ec 'object-delete (object-id object)))
						(objects wr (lambda (object)
							(not (or ; TODO Don't remove nearest objects?
								(protagonist? object)
								(member (object-id object) party =)
							))
						))
					)
				)
			)
		))
	)

	(define (packet-handler/logout ec wr packet)
		(trigger ec 'logout)
	)

	(define handlers-table (make-hash (list
		(cons #x01 (cons game-server-packet/move-to-point packet-handler/move-to-point)) ; change-moving
		(cons #x03 (cons game-server-packet/char-info packet-handler/char-info)) ; creature-create
		(cons #x04 (cons game-server-packet/user-info packet-handler/user-info)) ; creature-create
		(cons #x05 (cons game-server-packet/attack packet-handler/attack)) ; attack
		(cons #x06 (cons game-server-packet/die packet-handler/die)) ; die
		(cons #x07 (cons game-server-packet/revive packet-handler/revive)) ; revive
		; #x0a AttackCanceld
		(cons #x0b (cons game-server-packet/spawn-item packet-handler/spawn-item)) ; item-spawn
		(cons #x0c (cons game-server-packet/drop-item packet-handler/drop-item)) ; item-spawn
		(cons #x0d (cons game-server-packet/pick-item packet-handler/pick-item)) ; item-pick
		(cons #x0e (cons game-server-packet/status-update packet-handler/status-update)) ; creature-update
		; #x10 SellList
		(cons #x12 (cons game-server-packet/object-deleted packet-handler/object-deleted)) ; object-delete
		; #x13 CharSelectInfo
		; #x15 CharSelected
		; #x17 CharTemplates
		(cons #x16 (cons game-server-packet/npc-info packet-handler/npc-info)) ; creature-create
		(cons #x1b (cons game-server-packet/inventory-info packet-handler/inventory-info))
		; #x25 ActionFailed
		(cons #x27 (cons game-server-packet/inventory-update packet-handler/inventory-update))
		(cons #x28 (cons game-server-packet/teleport packet-handler/teleport)) ; teleport
		(cons #x29 (cons game-server-packet/target-selected packet-handler/target-selected)) ; change-target
		(cons #x2a (cons game-server-packet/target-unselected packet-handler/target-unselected)) ; change-target
		; #x2b ? AutoAttackStart
		; #x2c ? AutoAttackStop
		(cons #x2d (cons game-server-packet/social-action packet-handler/social-action)) ; gesture
		(cons #x2e (cons game-server-packet/change-move-type packet-handler/change-move-type)) ; change-walking
		(cons #x2f (cons game-server-packet/change-wait-type packet-handler/change-wait-type)) ; change-sitting
		(cons #x32 (cons game-server-packet/ask-join-clan packet-handler/ask-join-clan)) ; ask
		(cons #x39 (cons game-server-packet/ask-join-party packet-handler/ask-join-party)) ; ask
		; (cons #x3a (cons game-server-packet/reply-join-party packet-handler/reply-join-party)) ; reject
		; (cons #x45 (cons game-server-packet/shortcut-init packet-handler/shortcut-init))
		(cons #x47 (cons game-server-packet/stop-moving packet-handler/stop-moving)) ; change-moving
		(cons #x48 (cons game-server-packet/skill-started packet-handler/skill-started)) ; skill-started
		(cons #x49 (cons game-server-packet/skill-canceled packet-handler/skill-canceled)) ; skill-canceled
		(cons #x4a (cons game-server-packet/chat-message packet-handler/chat-message)) ; message
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
		; #x63 ?
		; (cons #x64 (cons game-server-packet/system-message packet-handler/system-message)) ; system-message (especial handling).
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
		(cons #xa6 (cons game-server-packet/my-target-selected packet-handler/my-target-selected)) ; change-target
		(cons #xa7 (cons game-server-packet/party-member-position packet-handler/party-member-position)) ; creature-update
		(cons #xa8 (cons game-server-packet/ask-join-alliance packet-handler/ask-join-alliance)) ; ask
		; #xc4 Earthquake
		; #xc8 NormalCamera
		; #xce ?
		; #xd0 MultiSellList
		; #xd3 ?
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
