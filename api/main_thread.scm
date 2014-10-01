(module api racket/base
	(require
		srfi/1
		racket/async-channel
		"../library/structure.scm"
		;"../library/network.scm"
		"../library/system.scm"
	)
	(provide main-thread)

	
	(define (make-event name . tail)
		(define data (if (null? tail) (list) (car tail)))
		(cons (cons 'name name) data)
	)
	
	(define (make-command-channel)
		(wrap-evt
			(thread-receive-evt)
			(lambda (handle) (cons 'command (thread-try-receive)))
		)
	)
	
	(define (make-packet-channel connection)
		(wrap-evt 
			(get-box-field connection 'input-channel)
			(lambda (buffer) (cons 'packet buffer))
		)
	)
	(define (make-timer-channel name time repeat?)
		(wrap-evt
			(alarm-evt (+ (current-inexact-milliseconds) time))
			(lambda (handle) (list (if repeat? 'interval 'timeout) name time))
		)
	)
	
	
	(define (main-thread connection)
		(define output (get-box-field connection 'events))
		(define input (make-packet-channel connection))
		(define command (make-command-channel))
		(define timers (list))
		(define events (list))
		
		(let loop ()
			(let ((event (apply sync (append (list input command) (map cdr timers)))))
				;(displayln event)
				(case (first event)
					((packet)
						;(async-channel-put output "@packet")
						
						; TODO Получаем пакет и обрабатываем. Если с ним связана генерация события, то возвращаем событие, иначе читаем дальше
						;(case (get-packet-id buffer)
						;	((#x00) (let ((packet (game-server-packet/? buffer)))
						;		
						;	))
						;	(else (loop))
						;)
					)
					((timeout) (let-values (((name time) (apply values (cdr event))))
						(async-channel-put output (make-event name))
						(set! timers (alist-delete name timers equal?)) ; clean
					))
					((interval) (let-values (((name time) (apply values (cdr event))))
						(let ((timer (make-timer-channel name time #t))) ; renew
							(async-channel-put output (make-event name))
							(set! timers (alist-cons name timer (alist-delete name timers equal?)))
						)
					))
					((command) (case (second event)
						((set-timeout) (let-values (((name time) (apply values (cddr event))))
							(set! timers (append
								(if time (list (cons name (make-timer-channel name time #f))) (list))
								(alist-delete name timers equal?)
							))
						))
						((set-interval) (let-values (((name time) (apply values (cddr event))))
							(set! timers (append
								(if time (list (cons name (make-timer-channel name time #t))) (list))
								(alist-delete name timers equal?)
							))
						))
					))
				)
			)
			
			(loop)
		)
	)	
)