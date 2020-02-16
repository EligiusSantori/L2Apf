(module system racket/base
	(require
		racket/contract
		racket/function
		racket/async-channel
		"connection.scm"
	)
	(provide (contract-out
		(make-event (->* (symbol?) #:rest list? pair?))
		(event? (-> any/c boolean?))
		(event-name (-> event? symbol?))
		(trigger! (-> connection? event? symbol?))
		(next-tick! (-> connection? procedure? void?))
		(alarm! (->* (connection? integer?) #:rest list? symbol?))
		(timeout! (->* (connection? integer?) #:rest list? symbol?))
		(interval! (->* (connection? integer?) #:rest list? symbol?))
		(stop! (-> connection? symbol? symbol?))
	))

	(define (make-event name . data)
		(cons name data)
	)
	(define (event? e)
		(and (list? e) (not (null? e)) (symbol? (car e)))
	)
	(define (event-name e)
		(car e)
	)

	(define (trigger! cn e)
		(async-channel-put (connection-event-channel cn) e)
		(event-name e)
	)

	(define (next-tick! cn tick)
		(async-channel-put (connection-tick-channel cn) tick)
	)

	(define (alarm! cn time . data)
		(let ((id (gensym)))
			(thread-send (connection-timer-thread cn)
				(cons id (wrap-evt (alarm-evt time) (const (apply make-event id data))))
			#f)
			id
		)
	)
	(define (timeout! cn timeout . data)
		(apply alarm! cn (+ timeout (current-inexact-milliseconds)) data)
	)
	(define (interval! cn interval . data)
		(define (setup start interval counter cn id data)
			(thread-send (connection-timer-thread cn) (cons id (wrap-evt
				(alarm-evt (+ start (* counter interval)))
				(lambda args
					(setup start interval (+ 1 counter) cn id data)
					(apply make-event id data)
				)
			)) #f)
		)
		(let ((id (gensym)))
			(setup (current-inexact-milliseconds) interval 1 cn id data)
			id
		)
	)
	(define (stop! cn timer)
		(thread-send (connection-timer-thread cn) timer)
		timer
	)
)
