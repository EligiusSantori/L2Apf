(module system racket/base
	(require
		(for-syntax racket/base)
		racket/contract
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
		(alarm! (->* (connection? rational?) (#:id symbol? event?) symbol?))
		(timeout! (->* (connection? rational?) (#:id symbol? event?) symbol?))
		(interval! (->* (connection? rational?) (#:id symbol? event?) symbol?))
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

	(define (alarm! #:id [id (gensym)] cn time [ev #f])
		(thread-send (connection-timer-thread cn)
			(list id time (or ev (make-event id)))
		#f)
		id
	)
	(define (timeout! #:id [id (gensym)] cn timeout [ev #f])
		(alarm! #:id id cn (+ (timestamp) timeout) ev)
	)
	(define (interval! #:id [id (gensym)] cn interval [ev #f])
		(thread-send (connection-timer-thread cn)
			(list id (cons (timestamp) interval) (or ev (make-event id)))
		#f)
		id
	)
	(define (timer-stop! cn timer)
		(let ((th (connection-timer-thread cn)))
			(if (thread-running? th)
				(begin (thread-send th timer) timer)
				#f
			)
		)
	)

	(define-syntax case-event
		(syntax-rules (else)
			((_ EV) (void))
			((_ EV (else . BODY))
				(begin . BODY)
			)
			((_ EV (ID ARGS . BODY) . REST)
				(if (eq? (event-name EV) ID)
					(apply (lambda ARGS . BODY) (cdr EV))
					(case-event EV . REST)
				)
			)
		)
	)
)
