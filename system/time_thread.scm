(module system racket/base
	(require
		srfi/1
		racket/async-channel
		"../library/structure.scm"
		"make_event.scm"
	)
	(provide time-thread)

	(define (make-command-channel)
		(wrap-evt
			(thread-receive-evt)
			(lambda (handle)
				(cons 'command (thread-try-receive))
			)
		)
	)
	
	(define (make-timeout-channel name timeout)
		(wrap-evt
			(alarm-evt timeout)
			(lambda (handle)
				(list 'timeout name timeout)
			)
		)
	)
	
	(define (make-interval-channel name fn timeout)
		(wrap-evt
			(alarm-evt timeout)
			(lambda (handle)
				(list 'interval name fn)
			)
		)
	)
	
	(define (time-thread connection)
		(define channel (get-box-field connection 'time-channel))
		(define command (make-command-channel))
		(define timers (list))
		
		(define (set-interval name fn)
			(set! timers (append
				(let ((timeout (if fn (+ (current-inexact-milliseconds) (fn)) #f)))
					(if timeout (list (cons name (make-interval-channel name fn timeout))) (list))
				)
				(alist-delete name timers)
			))
		)
		
		(let loop ()
			(let ((event (apply sync (cons command (map cdr timers)))))
				(case (car event)
					((timeout) (let-values (((name timeout) (apply values (cdr event))))
						(async-channel-put channel (make-event name))
						(set! timers (alist-delete name timers)) ; clean
					))
					((interval) (let-values (((name fn) (apply values (cdr event))))
						(async-channel-put channel (make-event name))
						(set-interval name fn) ; renew
					))
					((command) (case (second event)
						((set-timeout) (let-values (((name timeout) (apply values (cddr event))))
							(set! timers (append
								(if timeout (list (cons name (make-timeout-channel name timeout))) (list))
								(alist-delete name timers)
							))
						))
						((set-interval) (let-values (((name fn) (apply values (cddr event))))
							(set-interval name fn)
						))
					))
				)
			)
			(loop)
		)
	)
)
