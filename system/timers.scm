(module system racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
		racket/function
		racket/async-channel
		"../library/structure.scm"
		"make_event.scm"
	)
	(provide (contract-out
		(set-timeout! (box? symbol? (or/c integer? false/c) . -> . void?))
		(set-interval! (box? symbol? (or/c integer? false/c) . -> . void?))
		(time-thread (box? . -> . all/c))
	))

	(define (set-timeout! connection name timeout) ; precise timeout
		(let ((timeout (if timeout (+ (current-inexact-milliseconds) timeout) #f)))
			(let ((thread (@: connection 'time-thread)))
				(thread-send thread (list 'set-timeout name timeout))
				(void)
			)
		)
	)
	
	(define (set-interval! connection name time-or-fn) ; depend on loading
		(let ((fn (if (integer? time-or-fn) (const time-or-fn) time-or-fn))) ; linear function
			(let ((thread (@: connection 'time-thread)))
				(thread-send thread (list 'set-interval name fn))
				(void)
			)
		)
	)
	
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
	
	(define (make-interval-channel name fn period)
		(wrap-evt
			(alarm-evt (+ (current-inexact-milliseconds) period))
			(lambda (handle)
				(list 'interval name fn period)
			)
		)
	)
	
	(define (time-thread connection)
		(define channel (@: connection 'time-channel))
		(define command (make-command-channel))
		(define timers (list))
		
		(define (set-interval! name fn last)
			(let ((period (if fn (fn last) #f)))
				(set! timers (append
					(if period (list (cons name (make-interval-channel name fn period))) (list))
					(alist-delete name timers)
				))
			)
		)
		
		(let loop ()
			(let ((event (apply sync (cons command (map cdr timers)))))
				(case (car event)
					((timeout) (let-values (((name timeout) (apply values (cdr event))))
						(async-channel-put channel (make-event name))
						(set! timers (alist-delete name timers)) ; clean
					))
					((interval) (let-values (((name fn period) (apply values (cdr event))))
						(async-channel-put channel (make-event name))
						(set-interval! name fn period) ; renew
					))
					((command) (case (second event)
						((set-timeout) (let-values (((name timeout) (apply values (cddr event))))
							(set! timers (append
								(if timeout (list (cons name (make-timeout-channel name timeout))) (list))
								(alist-delete name timers)
							))
						))
						((set-interval) (let-values (((name fn) (apply values (cddr event))))
							(set-interval! name fn #f)
						))
					))
				)
			)
			(loop)
		)
	)
)
