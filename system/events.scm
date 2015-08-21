(module system racket/base
	(require
		srfi/1
		(rename-in racket/contract (any all/c))
		racket/async-channel
		"../library/structure.scm"
		(only-in "../library/network.scm" get-packet-id disconnect)
		"make_event.scm"
		"handlers.scm"
	)

	(provide (contract-out
		(listen-event! (->* (box? symbol? (or/c procedure? false/c)) #:rest (or/c false/c (listof any/c)) void?))
		(trigger-event (box? symbol? list? . -> . void?))
		(make-event-channel (box? . -> . evt?))
	))
	
	(define (listen-event! connection name checker . tail)
		(define handler (if (null? tail) values (car tail)))
		(let ((events (alist-delete name (@: connection 'events))))
			(set-box-field! connection 'events
				(if checker (cons (list name checker handler) events) events)
			)
			(void)
		)
	)
	
	(define (set-proxy-event! connection name . tail)
		(define handler (if (null? tail) values (car tail)))
		(define (checker event) (equal? name (car event)))

		(let ((proxy (gensym)))
			(listen-event! connection proxy checker handler)
			proxy
		)
	)
	
	(define (trigger-event connection name . data)
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
