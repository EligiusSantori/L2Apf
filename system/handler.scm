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
			"magic_effects.scm"
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
			"attack_start.scm"
			"attack_stop.scm"
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

	(struct exn:error:handler exn:fail:user ()
		#:extra-constructor-name make-exn:error:handler
		#:transparent
	)
	(define (raise-handler-error message . args)
		(raise (make-exn:error:handler (error-format message args) (current-continuation-marks)))
	)
	(define (trigger ec name . data) (async-channel-put ec (apply make-event (cons name data))) name); async-channel? symbol? any/c ...
	; (define (trigger-change-moving! cn object-id from to) (trigger cn 'change-moving object-id from to)) ; connection? point? point?
	; (define (trigger-creature-create! cn creature) (trigger cn 'creature-create creature)) ; connection? creature?

	(define delayed-stop-moving (gensym))
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

	(define (resolve-creature creature-or-type data wr db)
		(let ((id (ref data 'object-id)))
			(let ((creature (or (and (not (symbol? creature-or-type)) creature-or-type) (object-ref wr id))))
				(cond
					((and creature (protagonist? creature))
						(let ((changes (update-protagonist! creature data)))
							(if (and id (not (object-ref wr id)))
								(begin
									(register-object! wr creature)
									(values creature #f)
								)
								(values creature changes)
							)
						)
					)
					((and creature (character? creature)) (values creature (update-character! creature data)))
					((and creature (npc? creature)) (values creature (update-npc! creature data)))
					((and (symbol? creature-or-type) (eq? creature-or-type 'character))
						(let ((character (make-antagonist data db)))
							(register-object! wr character)
							(values character #f)
						)
					)
					((and (symbol? creature-or-type) (eq? creature-or-type 'npc))
						(let ((npc (make-npc data db)))
							(register-object! wr npc)
							(values npc #f)
						)
					)
					(creature (raise-handler-error "Unknown creature type." creature))
					(else (raise-handler-error "Creature is missed." id))
				)
			)
		)
	)
	(define (handle-creature-update! ec wr db data [creature-or-type #f])
		(let-values (((creature changes) (resolve-creature creature-or-type (filter pair? data) wr db))) (cond
			(changes
				; On change target.
				(let ((change (ref changes 'target-id))) (when change
					(trigger ec 'change-target (object-id creature) (car change) (cdr change))
				))

				; On start/stop moving.
				(let ((pc (assq 'position changes)) (dc (assq 'destination changes)))
					(when (or pc dc) ; If position validation or start/stop moving.
						(let ((d (ref creature 'destination)) (p (ref creature 'position)) (s (get-move-speed creature)))
							(when (and dc (or (not d) (> s 0))) ; If start moving and speed known (can be 0 if max speed missed) or stop moving.
								(trigger ec 'change-moving (object-id creature) p d)
							)
							(let ((wf (and d (> s 0) (+ (ref creature 'located-at) (/ (distance/3d p d) s))))) ; Calculate finish time if moving.
								(trigger ec delayed-stop-moving (object-id creature) wf d (ref creature 'moving-timer))
							)
						)
					)
				)

				; On walk/run.
				(let ((change (assoc 'walking? changes eq?))) (when change
					(trigger ec 'change-walking (object-id creature) (cadr change))
				))

				; On sit/stand.
				(let ((change (assoc 'sitting? changes eq?))) (when change
					(trigger ec 'change-sitting (object-id creature) (cadr change))
				))

				; On field change.
				(let ((changes (alist-except changes eq? 'target-id 'casting 'position 'destination 'angle 'walking? 'sitting?)))
					(when (not (null? changes))
						(trigger ec 'creature-update (object-id creature) changes)
					)
				)

				creature
			)
			(creature
				(trigger ec 'creature-create (object-id creature))

				creature
			)
			(else #f) ; Creature not found.
		))
	)

	; Arguments: connection, event-channel, packet-channel, tick-channel
	(define (handle-interrupt cn ec pc tc)
		(let ((wr (connection-world cn)) (db (connection-db cn)) (ev (sync/enable-break ec pc (connection-read-thread cn))))
			(cond
				((bytes? ev) ; Incoming packet => handle then wait next.
					(if (= (get-packet-id ev) #x64)
						(packet-handler/system-message cn ec wr db (game-server-packet/system-message ev)) ; Special case.
						(let ((row (hash-ref handlers-table (get-packet-id ev) #f)))
							(if row
								(with-handlers ([exn:error:handler? (lambda (e)
										(apf-error "Error \"~a\" in handler for #x~a (~v)." (exn-message e) (byte->hex (get-packet-id ev)) cn)
									)])
    								((cdr row) ec wr db ((car row) ev))
								)
								(apf-debug "Unhandled packet ~v" ev) ; TODO apf-warn
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
			('creature-update (id changes) ; Stop casting on knockout.
				(when (alist-contains changes eq? 'stunned? 'sleeping? 'silenced? 'paralyzed?)
					(let ((creature (object-ref wr id))) (when creature
						(let ((skill (ref creature 'casting))) (when skill
							(update-creature! creature (list
								(cons 'casting #f)
								(cons 'located-at (timestamp)) ; FIXME NO REASON, VALUE WILL BE DROPPED.
							))

							(trigger! cn (make-event 'skill-canceled id skill))
						))
					))
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
						(when (ref skill 'harmful?)
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
					(when (and (member (object-id me) target-ids =) (ref skill 'harmful?))
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
			('change-moving (subject-id position destination) ; Finish moving by timer.
				(when (not destination)
					(let ((subject (object-ref wr subject-id)))
						(when (and (creature? subject) (ref subject 'destination))
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
				(timer-stop! cn timer) ; Reset stop moving timer.
				(when will-finish ; If moving and speed known.
					(let ((ev (make-event 'change-moving subject-id destination #f)))
						(if (> will-finish (timestamp))
							(alarm! #:id timer cn will-finish ev) ; Set new stop moving timer.
							(trigger! cn ev) ; Trigger immediately if time is already up.
						)
					)
				)
				#f
			)
			(else ev)
		)
	)

	(define (packet-handler/user-info ec wr db packet)
		(handle-creature-update! ec wr db packet (world-me wr))
	)
	(define (packet-handler/char-info ec wr db packet)
		(handle-creature-update! ec wr db packet 'character)
	)
	(define (packet-handler/npc-info ec wr db packet)
		(handle-creature-update! ec wr db packet 'npc)
	)
	(define (packet-handler/status-update ec wr db packet)
		(handle-creature-update! ec wr db packet)
	)
	(define (packet-handler/magic-effects ec wr db packet)
		(printf "{magic-effects ~a}~n" packet)(flush-output)
		; (define (make-effects data)
		; 	(fold (lambda ()
		;
		; 	) #f data)
		; )
		; (let ((new (make-effects (ref packet 'effects))) (old (ref me 'effects)))
		;
		; )
		;
		;
		; (trigger ec 'creature-update (object-id me) (cons 'effects (cons new old)))
	)

	(define (packet-handler/inventory-info ec wr db packet)
		(let ((inv (world-inventory wr)))
			(hash-clear! inv)
			(map (lambda (item)
				(hash-set! inv (ref item 'object-id) (make-item item))
			) (ref packet 'items))
		)
	)
	(define (packet-handler/inventory-update ec wr db packet)
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
	(define (packet-handler/spawn-item ec wr db packet)
		(let ((item (make-item packet)))
			(register-object! wr item)
			(trigger ec 'item-spawn (object-id item) #f)
		)
	)
	(define (packet-handler/drop-item ec wr db packet)
		(let ((item (make-item packet)))
			(register-object! wr item)
			(trigger ec 'item-spawn (object-id item) (ref packet 'subject-id))
		)
	)
	(define (packet-handler/pick-item ec wr db packet) ; Происходит перед object-delete.
		(trigger ec 'item-pick (ref packet 'object-id) (ref packet 'subject-id) (ref packet 'position))
	)
	(define (packet-handler/object-deleted ec wr db packet)
		(let* ((id (ref packet 'object-id)) (object (object-ref wr id)))
			(when (creature? object) ; Clear targets to deleted object.
				(objects wr (lambda (subject)
					(when (and (creature? subject) (equal? (ref subject 'target-id) id))
						(handle-creature-update! ec wr db (list (cons 'target-id #f)) subject)
					)
				))
			)
			(trigger ec 'object-delete id)
		)
	)

	(define (packet-handler/move-to-point ec wr db packet) ; Происходит, когда существо бежит к точке.
		(handle-creature-update! ec wr db packet)
	)
	(define (packet-handler/move-to-pawn ec wr db packet) ; Происходит, когда существо бежит к другому существу или когда начинается каст (возможно атака тоже).
		(let ((creature (object-ref wr (ref packet 'object-id))) (target (object-ref wr (ref packet 'target-id))))
; (when (antagonist? creature) (printf "<~a> {move-to-pawn ~a}~n" (format-time) packet)(flush-output))
			(if (and creature target)
				(handle-creature-update! ec wr db (if (npc? creature)
					(begin
						(list
							(cons 'object-id (object-id creature))
							(cons 'target-id (object-id target))
							(assq 'position packet)
							(cons 'destination (get-position target))
						)
					)
					(list
						(cons 'object-id (object-id creature))
						(assq 'position packet)
					)
				) creature)
				(apf-warn "World object ~v is missed, handler: move-to-pawn." (ref packet 'target-id))
			)
		)
	)
	(define (packet-handler/validate-location ec wr db packet)
		(handle-creature-update! ec wr db packet)
	)
	(define (packet-handler/stop-moving ec wr db packet)
		(handle-creature-update! ec wr db (cons (cons 'destination #f) packet))
	)
	(define (packet-handler/change-move-type ec wr db packet)
		(let ((creature (object-ref wr (ref packet 'object-id)))) (when creature
			(handle-creature-update! ec wr db (list
				(cons 'object-id (object-id creature))
				(cons 'position (get-position creature))
				(assq 'walking? packet)
			))
		))
	)
	(define (packet-handler/change-wait-type ec wr db packet)
		(handle-creature-update! ec wr db (cons (cons 'destination #f) packet))
	)

	(define (packet-handler/target-selected ec wr db packet)
		(handle-creature-update! ec wr db packet)
	)
	(define (packet-handler/target-unselected ec wr db packet)
		(handle-creature-update! ec wr db (cons (cons 'target-id #f) packet))
	)
	(define (packet-handler/my-target-selected ec wr db packet)
		(handle-creature-update! ec wr db (list
			(cons 'object-id (object-id (world-me wr)))
			(assq 'target-id packet)
		))
	)

	(define (packet-handler/attack-start ec wr db packet)
		(handle-creature-update! ec wr db (list
			(assq 'object-id packet)
			(cons 'in-combat? #t)
		))
	)
	(define (packet-handler/attack-stop ec wr db packet)
		(handle-creature-update! ec wr db (list
			(assq 'object-id packet)
			(cons 'in-combat? #f)
		))
	)
	(define (packet-handler/attack ec wr db packet)
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
								(handle-creature-update! ec wr db (list
									(and hp (cons 'hp (max 0 (- hp damage))))
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
				(handle-creature-update! ec wr db (list ; Fix missing change-moving event & subject in-combat? state.
					(assq 'object-id packet)
					(assq 'position packet)
					(cons 'destination #f)
					(cons 'last-attack (timestamp))
					(if in-combat? (cons 'in-combat? #t) #f)
					(if (not (assq (ref subject 'target-id) hits))
						(assq 'target-id (car (ref packet 'hits)))
						#f
					)
				) subject)

				(trigger ec 'attack (object-id subject) hits) ; Trigger main event.
			)
		))
	)
	(define (packet-handler/social-action ec wr db packet)
		(trigger ec 'gesture (ref packet 'object-id) (ref packet 'action))
	)

	(define (packet-handler/skill-list ec wr db packet)
		(let ((skills (world-skills wr)))
			(hash-clear! skills)
			(apply hash-set*! skills
				(fold (lambda (s r)
					(let ((skill (make-skill s db)))
						(cons (skill-id skill) (cons skill r))
					)
				) (list) (ref packet 'list))
			)
		)
	)
	(define (packet-handler/skill-started ec wr db packet)
		(define (update-or-create-skill packet)
			(or
				(and (= (object-id (world-me wr)) (ref packet 'object-id))
					(let ((skill (skill-ref wr (ref packet 'skill-id)))) (and skill (begin
						(update-skill! skill (list
							(assq 'level packet)
							(and (ref skill 'toggle?)
								(cons 'enabled? (not (ref skill 'enabled?)))
							)
							(cons 'last-usage (timestamp))
							(assq 'reuse-delay packet)
						))
						skill
					)))
				)
				(make-skill (list
					(assq 'skill-id packet)
					(assq 'level packet)
					(cons 'active? #t)
					(cons 'last-usage (timestamp))
					(assq 'reuse-delay packet)
				) db)
			)
		)

		(let ((creature (object-ref wr (ref packet 'object-id)))) (when creature
			(let ((skill (update-or-create-skill packet)) (me (world-me wr)))
				(handle-creature-update! ec wr db (list
					(cons 'object-id (object-id creature))
					; (cons 'target-id (ref packet 'target-id)) ; It's possible to cast on self without target changing.
					(assq 'position packet)
					(cons 'destination #f)
					(and (not (ref skill 'toggle?))
						(cons 'casting skill)
					)
				) creature)

				(trigger ec 'skill-started (object-id creature) (ref packet 'target-id) skill)
			)
		))
	)
	(define (packet-handler/skill-launched ec wr db packet)
		(define (find-or-create-skill creature packet)
			(or
				(ref creature 'casting)
				(and (protagonist? creature)
					(skill-ref wr (ref packet 'skill-id))
				)
				(make-skill (list
					(assq 'skill-id packet)
					(assq 'level packet)
					(cons 'active? #t)
				) db)
			)
		)

		(let ((creature (object-ref wr (ref packet 'object-id)))) (when creature
			(let ((skill (find-or-create-skill creature packet)))
				(update-creature! creature (list
					(cons 'casting #f)
					(cons 'located-at (timestamp)) ; FIXME NO REASON, VALUE WILL BE DROPPED.
				))

				(trigger ec 'skill-launched (object-id creature) skill (ref packet 'targets))
			)
		))
	)
	(define (packet-handler/skill-canceled ec wr db packet)
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

	(define (packet-handler/ask-join-clan ec wr db packet)
		(trigger ec 'ask/join-clan (ref packet 'name))
	)
	(define (packet-handler/ask-join-party ec wr db packet)
		(trigger ec 'ask/join-party (ref packet 'name) (ref packet 'loot))
	)
	(define (packet-handler/ask-be-friends ec wr db packet)
		(trigger ec 'ask/be-friends (ref packet 'name))
	)
	(define (packet-handler/ask-join-alliance ec wr db packet)
		(trigger ec 'ask/join-alliance (ref packet 'name))
	)
	(define (packet-handler/reply-join-party ec wr db packet)
		(when (not (ref packet 'accept?))
			(trigger ec 'reject/join-party
				; (cons 'object-id ?) ; TODO
				; (cons 'name ?) ; TODO?
			)
		)
	)

	(define (packet-handler/party-all-members ec wr db packet)
		(let ((me (world-me wr)) (leader-id (ref packet 'leader-id)))
			(set-world-party! wr (apply make-party
				(ref packet 'loot-mode)
				leader-id
				(fold (lambda (data members)
					(let ((character (handle-creature-update! ec wr db data 'character)))
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
	(define (packet-handler/party-add-member ec wr db packet)
		(let ((character (handle-creature-update! ec wr db packet 'character)))
			(when (not (in-party? (world-party wr) (object-id character)))
				(party-add! wr (object-id character))
				(trigger ec 'party-memeber-join (object-id character))
			)
		)
	)
	(define (packet-handler/party-member-update ec wr db packet)
		(handle-creature-update! ec wr db packet 'character)
	)
	(define (packet-handler/party-member-position ec wr db packet)
		(map (lambda (data) (handle-creature-update! ec wr db data)) (ref packet 'members))
	)
	(define (packet-handler/party-delete-member ec wr db packet)
		(let ((object-id (ref packet 'object-id)))
			(party-kick! wr object-id)
			(if (world-party wr)
				(trigger ec 'party-memeber-leave object-id)
				(trigger ec 'party-leave)
			)
			(trigger ec 'party-memeber-leave object-id)
		)
	)
	(define (packet-handler/party-clear-members ec wr db packet)
		(when (world-party wr)
			(party-clear! wr)
			(trigger ec 'party-leave)
		)
	)

	(define (packet-handler/chat-message ec wr db packet)
		(trigger ec 'message
			(ref packet 'object-id)
			(ref packet 'channel)
			(ref packet 'author)
			(ref packet 'text)
		)
	)
	(define (packet-handler/system-message cn ec wr db packet)
		(let ((message-id (ref packet 'message-id)) (arguments (ref packet 'arguments)))
			(cond
				((or (= message-id 612) (= message-id 357)) ; Set spoiled state here bacause there is no other way.
					(handle-creature-update! ec wr db (list
						(cons 'object-id (ref (world-me wr) 'target-id))
						(cons 'spoiled? #t)
					))
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
				; ((= message-id 110)) ; You can feel <skill-id>'s effect.
			)

			; TODO Set is effect failed?

			(trigger ec 'system-message message-id arguments)
		)
	)
	(define (packet-handler/confirm ec wr db packet)
		(let ((arguments (ref packet 'arguments)))
			(case (ref packet 'message-id)
				((1510)
					(trigger ec 'confirm/resurrect
						(ref arguments 'text)
						(ref arguments 'number)
					)
				)
			)
		)
	)

	(define (packet-handler/die ec wr db packet)
		(let ((creature (object-ref wr (ref packet 'object-id)))) (when creature
			(handle-creature-update! ec wr db (list
				(cons 'alike-dead? #t)
				(cons 'dead? #t)
				(cons 'hp 0)
				(assq 'spoiled? packet)

				; Fix missing change-moving event.
				(cons 'position (get-position creature))
				(cons 'destination #f)
			) creature)

			; Trigger main event.
			(trigger ec 'die (object-id creature) (and (protagonist? creature) (ref packet 'return)))
		))
	)
	(define (packet-handler/revive ec wr db packet)
		(define data (list
			(assq 'object-id packet)
			(cons 'alike-dead? #f)
			(cons 'dead? #f)
		))

		(let ((creature (handle-creature-update! ec wr db data))) (when creature
			(trigger ec 'revive (object-id creature))
		))
	)

	(define (packet-handler/teleport ec wr db packet)
		(let ((creature (handle-creature-update! ec wr db (cons (cons 'destination #f) packet)))) (when creature
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

	(define (packet-handler/logout ec wr db packet)
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
		(cons #x16 (cons game-server-packet/npc-info packet-handler/npc-info)) ; creature-create
		; #x17 CharTemplates
		(cons #x1b (cons game-server-packet/inventory-info packet-handler/inventory-info))
		; #x25 ActionFailed
		(cons #x27 (cons game-server-packet/inventory-update packet-handler/inventory-update))
		(cons #x28 (cons game-server-packet/teleport packet-handler/teleport)) ; teleport
		(cons #x29 (cons game-server-packet/target-selected packet-handler/target-selected)) ; change-target
		(cons #x2a (cons game-server-packet/target-unselected packet-handler/target-unselected)) ; change-target
		(cons #x2b (cons game-server-packet/attack-start packet-handler/attack-start)) ; creature-update
		(cons #x2c (cons game-server-packet/attack-stop packet-handler/attack-stop)) ; creature-update
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
		; #x63 FinishRotating
		; (cons #x64 (cons game-server-packet/system-message packet-handler/system-message)) ; system-message (especial handling).
		; #x65 StartPledgeWar
		; #x6d SetupGauge
		; #x6f ChooseInventoryItem
		(cons #x76 (cons game-server-packet/skill-launched packet-handler/skill-launched)) ; skill-launched
		(cons #x7d (cons game-server-packet/ask-be-friends packet-handler/ask-be-friends)) ; ask
		(cons #x7e (cons void packet-handler/logout)) ; logout
		; (cons #x7f (cons game-server-packet/magic-effects packet-handler/magic-effects)) ; effect (skill-id, level, duration)
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
		; #xce RelationChanged
		; #xd0 MultiSellList
		; #xd3 NetPing
		; #xd4 Dice
		; #xd5 Snoop
		; (cons #xe4 'henna-info (cons game-server-packet/henna-info packet-handler/henna-info))
		; (cons #xe7 'macro-list (cons game-server-packet/macro-list packet-handler/macro-list))
		(cons #xed (cons game-server-packet/system-message packet-handler/confirm))
		; #xee PartySpelled
		; #xf5 SSQStatus
		; (cons #xf8 'signs-sky (cons game-server-packet/signs-sky packet-handler/signs-sky)) SignsSky
		; #xfa L2FriendList
		; (cons #xfe 'Ex* (cons game-server-packet/ packet-handler/))

		; (cons #x?? game-server-packet/ packet-handler/)
	)))
)
