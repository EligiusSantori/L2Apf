(module event racket/base
	(require
		srfi/1
		"../library/ral.scm"
		"../library/structure.scm"
		"../library/logic.scm"
		"../system/events.scm"
		"../system/timers.scm"
	)
	(provide set-radar-event!)

	(define sampling-frequency 1000) ; const
	(define temp-interval-event (gensym))
	
	(define (set-radar-event! connection name radius . temp)
		(define suitable? (if (null? temp) object? (car temp)))
		(define (predicate k v) (and (integer? k) (suitable? v)))
	
		(let* ((world (@: connection 'world)) (me (@: world 'me)) (storage (make-hash)))
			(define (handle object)
				(let ((object-id (@: object 'object-id)))
					(unless (hash-has-key? storage object-id)
						(hash-set! storage object-id (cons #f object))
					)
					
					(let ((include? (car (hash-ref storage object-id))) (distance (objects-distance me object)))
						(cond
							((and (not include?) (<= distance radius))
								(hash-set! storage object-id (cons #t object))
								(run-event connection name (list
									(cons 'object object)
									(cons 'action 'come)
								))
							)
							((and include? (> distance radius))
								(hash-set! storage object-id (cons #f object))
								(run-event connection name (list
									(cons 'object object)
									(cons 'action 'leave)
								))
							)
						)
					)
				)
			)
			
			(define (get-missed objects)
				(let ((ids (lset-difference = (map (lambda (i) (@: i 'object-id)) objects) (hash-keys storage))))
					(map (lambda (object-id) (cdr (hash-ref storage object-id))) ids)
				)
			)
		
			(define (checker event)
				(when (equal? temp-interval-event (@: event 'name)) ; check if it's my interval event
					(let ((objects (hash-filter world predicate)))
						(map handle objects) ; handle new and known objects
						(let ((missed (get-missed objects)))
							(map (lambda (object) ; handle missed objects
								(run-event connection name (list
									(cons 'object object)
									(cons 'action 'leave)
								))
							) objects)
						)
					)
				)
				
				#f ; always false because events triggered by "run-event"
			)
			
			(set-interval! connection temp-interval-event sampling-frequency)
			(set-event! connection name checker)
		)
	)
)