(module system racket/base
	(require
		srfi/1
		data/queue
		racket/function
		racket/async-channel
		(rename-in racket/contract (any all/c))
		"../library/structure.scm"
		"make_event.scm"
	)
	(provide (contract-out
		(set-alarm! (->* (box? symbol? integer?) #:rest (listof any/c) symbol?))
		(set-timeout! (->* (box? symbol? integer?) #:rest (listof any/c) symbol?))
		(set-interval! (->* (box? symbol? integer?) #:rest (listof any/c) symbol?))
		(clear-timers! (->* (box?) #:rest (listof symbol?) void?))
		(time-thread (async-channel? . -> . all/c))
	))

	(define timers (make-hash)) ; TODO inner connection

	(define (set-alarm! connection name alarm . data)
		(let ((id (gensym)) (thread (@: connection 'time-thread)))
			(hash-set! timers id (wrap-evt
				(alarm-evt alarm)
				(lambda (e) (cons id (cons #f (cons name data))))
			))
			(thread-send thread 'break)
			id
		)
	)

	(define (set-timeout! connection name timeout . data)
		(let ((alarm (+ (current-inexact-milliseconds) timeout)))
			(apply set-alarm! (cons connection (cons name (cons alarm data))))
		)
	)

	(define (set-interval! connection name period . data) ; TODO нет возможности обнулить таймер. неточное время (с разрывами)
		(let ((id (gensym)) (thread (@: connection 'time-thread)))
			(hash-set! timers id (wrap-evt
				(alarm-evt (+ (current-inexact-milliseconds) period))
				(lambda (e) (cons id (cons period (cons name data))))
			))
			(thread-send thread 'break)
			id
		)
	)

	(define (clear-timers! connection . ids)
		(map (lambda (id) (hash-remove! timers id)) ids)
	)

	(define (time-thread channel)
		(let loop ()
			(let ((event (apply sync (cons (thread-receive-evt) (hash-values timers)))))
				(when (list? event)
					(apply (lambda (id period name . data)
						(if period
							(hash-set! timers id (wrap-evt
								(alarm-evt (+ (current-inexact-milliseconds) period))
								(lambda (e) (cons id (cons period (cons name data))))
							))
							(hash-remove! timers id)
						)
						(async-channel-put channel (apply make-event (cons name data)))
					) event)
				)
			)

			(loop)
		)
	)
)
