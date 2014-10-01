(module system racket/base
	(require
		srfi/1
		racket/async-channel
		"../library/structure.scm"
		"../library/network.scm"
		"../library/logic.scm"
		"../packet/game/server/chat_message.scm"
		"make_event.scm"
	)
	(provide main-thread)

	(define (make-command-channel)
		(wrap-evt
			(thread-receive-evt)
			(lambda (handle) (cons 'command (thread-try-receive)))
		)
	)
	
	(define (make-network-channel connection)
		(wrap-evt 
			(get-box-field connection 'input-channel)
			(lambda (buffer) (cons 'packet buffer))
		)
	)
	
	(define (make-timers-channel connection)
		(wrap-evt 
			(get-box-field connection 'time-channel)
			(lambda (event) (cons 'forward event))
		)
	)
	
	(define (main-thread connection)
		(define channel (get-box-field connection 'events))
		(define command (make-command-channel))
		(define network (make-network-channel connection))
		(define timers (make-timers-channel connection))
		(define events (list))
		
		(define (set-event name checker . tail)
			(define handler (if (null? tail) values (car tail)))
			(set! events (append
				(if checker
					(list name checker handler)
					(list)
				)
				(alist-delete name events)
			))
		)
		
		(define (handle-packet buffer)
			(case (get-packet-id buffer)
				((#x4a) (let ((packet (game-server-packet/chat-message buffer)))
					(define world (get-box-field connection 'world))
					(make-event 'message (list
						(cons 'object-id (get-field packet 'object-id))
						(cons 'channel (get-field packet 'channel))
						(cons 'author (get-field packet 'author))
						(cons 'text (get-field packet 'text))
					))
				))
				(else #f)
			)
		)
		
		(let loop ()
			(let ((event (sync command network timers)))
				(case (car event)
					((packet)
						(let ((event (handle-packet (cdr event))))
							(when event
								(async-channel-put channel event)
								(map ; Инициируем все пользовательские события
									(lambda (e) (async-channel-put channel ((third e) event)))
									(filter (lambda (e) ((second e) event)) events)
								)
							)
						)
					)
					((forward)
						(async-channel-put channel (cdr event))
					)
					((command) (case (second event)
						((set-event) (apply set-event (cddr event)))
					))
				)
			)
			
			(loop)
		)
	)	
)
