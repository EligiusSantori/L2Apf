(module system racket/base
	(require
		(for-syntax racket/base)
		racket/contract
		racket/function
		racket/async-channel
		"../library/date_time.scm"
		"connection.scm"
	)
	(provide (contract-out
		(make-event (->* (symbol?) #:rest list? pair?))
		(event? (-> any/c boolean?))
		(event-name (-> event? symbol?))
		(trigger! (-> connection? event? symbol?))
		(next-tick! (-> connection? procedure? void?))
		(alarm! (->* (connection? integer?) (#:id symbol?) #:rest list? symbol?))
		(timeout! (->* (connection? integer?) (#:id symbol?) #:rest list? symbol?))
		(interval! (->* (connection? integer?) (#:id symbol?) #:rest list? symbol?))
		(timer-stop! (-> connection? symbol? symbol?))
	))
	(provide case-event)

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

	(define (alarm! #:id [id (gensym)] cn time . data)
		(thread-send (connection-timer-thread cn)
			(cons id (wrap-evt (alarm-evt (* time 1000)) (const (apply make-event id data))))
		#f)
		id
	)
	(define (timeout! #:id [id (gensym)] cn timeout . data)
		(apply alarm! #:id id cn (+ timeout (timestamp)) data)
	)
	(define (interval! #:id [id (gensym)] cn interval . data)
		(define (setup start interval counter cn id data)
			(thread-send (connection-timer-thread cn) (cons id (wrap-evt
				(alarm-evt (* (+ start (* counter interval)) 1000))
				(lambda args
					(setup start interval (+ 1 counter) cn id data)
					(apply make-event id data)
				)
			)) #f)
		)
		(setup (timestamp) interval 1 cn id data)
		id
	)
	(define (timer-stop! cn timer)
		(thread-send (connection-timer-thread cn) timer)
		timer
	)

	(define-syntax case-event
		(syntax-rules (else)
			((_ EV) (void))
			((_ EV (else . BODY))
				(begin . BODY)
			)
			((_ EV (ID ARGS . BODY) . REST)
				(if (eq? (event-name EV) (quote ID))
					(apply (lambda ARGS . BODY) (cdr EV))
					(case-event EV . REST)
				)
			)
		)
	)
)
