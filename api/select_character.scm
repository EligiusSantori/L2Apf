(module logic racket/base
	(require
		racket/function
		racket/async-channel
		racket/contract
		(relative-in "../."
			"library/extension.scm"
			"system/error.scm"
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
		"refresh_manor.scm"
		"refresh_quests.scm"
		"refresh_skills.scm"
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

						(refresh-manor cn)
						(refresh-quests cn)
						(send-packet cn (game-client-packet/enter-world))
					))
					(else (loop))
				)
			)
		)

		(let ((ec (make-async-channel)) (tc (make-async-channel)) (th (make-hasheq)) (pc (connection-packet-channel cn)))
			(set-connection-event-channel! cn ec)
			(set-connection-tick-channel! cn tc)
			(set-connection-timer-thread! cn (thread (lambda ()
				(define (next-interrupt) (apply sync
					(wrap-evt ; Contol event.
						(thread-receive-evt)
						(lambda args (thread-receive))
					)
					(hash-values th) ; List of alarm-evt.
				))
				(define (reset-interval! id ev start interval step)
					(hash-set! th id (wrap-evt (alarm-evt (* (+ start (* interval step)) 1000))
						(const (list id ev start interval (+ step 1)))
					))
				)

				(do ((evt (next-interrupt) (next-interrupt))) ((not evt))
					(cond
						((symbol? evt) ; Delete timer.
							(hash-remove! th evt)
						)
						((event? (cadr evt)) ; It's time.
							(let ((id (car evt)))
								(when (hash-has-key? th id) ; If timer wasn't removed.
									(if (null? (cddr evt))
										(hash-remove! th id) ; Delete expired timer.
										(apply reset-interval! evt) ; Reset interval.
									)
									(async-channel-put ec (cadr evt)) ; Trigger event.
								)
							)
						)
						((rational? (cadr evt)) ; Create alarm.
							(let-values (((id ts ev) (list->values evt)))
								(hash-set! th id (wrap-evt (alarm-evt (* ts 1000)) (const (list id ev))))
							)
						)
						(else ; Create interval.
							(let-values (((id st ev) (list->values evt)))
								(reset-interval! id ev (car st) (cdr st) 1)
							)
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
