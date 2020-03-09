(module logic racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
		racket/async-channel
		(relative-in "../."
			"library/extension.scm"
			"system/debug.scm"
			"system/structure.scm"
			"system/connection.scm"
			"system/event.scm"
			"system/handler.scm"
			; "system/timers.scm"
			(only-in "packet/packet.scm" get-packet-id)
			"packet/game/client/select_character.scm"
			"packet/game/server/player_character.scm"
			"packet/game/client/enter_world.scm"
			"model/protagonist.scm"
			"model/world.scm"
		)
		"refresh_manor_list.scm"
		"refresh_quest_list.scm"
		"refresh_skill_list.scm"
	)
	(provide (contract-out
		(select-character (-> connection? protagonist? evt?))
	))

	(define (select-character cn me)
		(send-packet cn (game-client-packet/select-character (list
			(cons 'id (ref me 'character-id))
		)))

		(let loop ()
			(let ((buffer (read-packet cn)))
				(case (get-packet-id buffer)
					((#x15) (let ((packet (game-server-packet/player-character buffer)))
						(update-protagonist! me packet)
						(set-world-me! (connection-world cn) me)

						(refresh-manor-list cn)
						(refresh-quest-list cn)
						(send-packet cn (game-client-packet/enter-world))
						(refresh-skill-list cn)
					))
					(else (loop))
				)
			)
		)

		(let ((ec (make-async-channel)) (tc (make-async-channel)) (ts (make-hasheq)) (pc (connection-packet-channel cn)))
			(set-connection-event-channel! cn ec)
			(set-connection-tick-channel! cn tc)
			(set-connection-timer-thread! cn (thread (lambda ()
				(define (next-interrupt) (apply sync
						(wrap-evt ; Contol event.
							(thread-receive-evt)
							(lambda args (thread-receive))
						)
						(hash-values ts) ; List of alarm-evt.
				))

				(do ((evt (next-interrupt) (next-interrupt))) ((not evt))
					(cond
						((event? evt) ; It's time.
							(let ((id (event-name evt)))
								(when (hash-has-key? ts id) ; If timer wasn't removed.
									(hash-remove! ts id) ; Delete expired timer.
									(async-channel-put ec evt) ; Trigger event.
								)
							)
						)
						((pair? evt) ; Create timer.
							(let-values (((id evt) (car+cdr evt)))
								(hash-set! ts id evt) ; Put timer into queue.
							)
						)
						((symbol? evt) ; Delete timer.
							(hash-remove! ts evt)
						)
					)
				)
			)))

			(wrap-evt always-evt (lambda args
				(let loop () ; Handle callbacks for current iteration.
					(let ((tick (async-channel-try-get tc)))
						(when tick (tick) (loop))
					)
				)
				(handle-interrupt cn ec pc tc)
			))
		)
	)
)
