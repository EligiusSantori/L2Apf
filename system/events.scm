(module system racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
		racket/async-channel
		"../library/structure.scm"
		(only-in "../library/network.scm" get-packet-id send disconnect)
		"../packet/game/client/appearing.scm"
		"../packet/game/client/validate_position.scm"
		"make_event.scm"
		"handlers.scm"
		"timers.scm"
		"../model/world.scm"
	)

	(provide (contract-out
		(listen-event! (->* (box? symbol? (or/c procedure? false/c)) (procedure?) void?))
		(trigger-event (box? symbol? list? . -> . void?))
		(make-event-channel (box? . -> . evt?))
	))

	; Set up or delete a custom event
	(define (listen-event! connection name checker [handler values])
		(let ((events (alist-delete name (@: connection 'events))))
			(set-box-field! connection 'events
				(if checker (cons (list name checker handler) events) events)
			)
			(void)
		)
	)
	
	; Trigger an event (enqueue to main channel)
	(define (trigger-event connection name . data) ; TODO just trigger
		(let ((channel (@: connection 'custom-channel)))
			(async-channel-put channel (apply make-event (cons name data)))
			(void)
		)
	)

	(define (make-event-channel connection)
		(let ((world (@: connection 'world)) (custom-channel (make-async-channel)))
			(define (handle b e)
				(let ((name (first b)) (data ((third b) e)))
					(async-channel-put custom-channel (apply make-event (if data (cons name data) (list name))))
					e
				)
			)
			(define (check e l)
				(filter (lambda (b) ((second b) e)) l)
			)

			(set-box-field! connection 'custom-channel custom-channel) ; custom events channel
			(set-box-field! connection 'events (list)) ; custom events list

			(listen-event! connection (gensym) ; disconnect via custom events
				(lambda (e) (equal? (car e) 'logout))
				(lambda (e) (begin (disconnect connection) #f))
			)
			(listen-event! connection (gensym)
				(lambda (e) (equal? (car e) 'teleport))
				(lambda (e)
					(let* ((me (@: connection 'world 'me)) (position (@: me 'position)) (angle (@: me 'angle)))
						(send connection (game-client-packet/validate-position position angle))
						(send connection (game-client-packet/appearing))
						#f
					)
				)
			)
			(listen-event! connection (gensym)
				(lambda (e) (and
					(equal? (car e) 'skill-started)
					(= (second e) (@: connection 'world 'me 'object-id))
				))
				(lambda (e)
					(let-values (((name object-id skill-id level) (apply values e)))
						(let ((skill (skill-ref (@: connection 'world) skill-id)))
							(when skill
								(let ((last-usage (@: skill 'last-usage)) (reuse-delay (@: skill 'reuse-delay)))
									(when (and reuse-delay (> reuse-delay 0))
										(set-alarm! connection 'skill-reused (+ last-usage reuse-delay) object-id skill-id level)
									)
								)
							)
						)
					)
					#f
				)
			)
			(listen-event! connection (gensym) ; fix position refreshing event on attack
				(lambda (e) (equal? (car e) 'attack))
				(lambda (e)
					(let* ((object-id (second e)) (creature (object-ref world object-id)))
						(when creature (apply trigger-event (list connection 'change-moving object-id (@: creature 'position) #f)))
					)
					#f
				)
			)

			(wrap-evt
				(choice-evt
					custom-channel
					(@: connection 'time-channel)
					(wrap-evt
						(@: connection 'input-channel)
						(lambda (buffer)
							(let ((row (hash-ref packet-handlers-table (get-packet-id buffer) #f)))
								(if row
									(let ((name (car row)) (data ((compose list (second row)) world buffer)))
										(apply make-event (if data (cons name data) (list name)))
									)
									(begin
										(displayln (format "unhandled packet #~x" (get-packet-id buffer)))
										(make-event 'nothing)
									)
								)
							)
						)
					)
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
