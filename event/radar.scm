(module event racket/base
	(require
		srfi/1
		"../library/extension.scm"
		"../library/structure.scm"
		"../logic/object.scm"
		"../system/events.scm"
		"../system/timers.scm"
	)
	(provide set-radar-event!)

	(define sampling-frequency 1000) ; const
	(define temp-interval-event (gensym))

	(define (set-radar-event! connection name radius . temp)
		(define suitable? (if (null? temp) object? (car temp)))

		(let* ((world (@: connection 'world)) (me (@: world 'me)) (storage (make-hash)))
			(define (handle-new-or-known pair)
				(let-values (((object-id object) (car+cdr pair)))
					(unless (hash-has-key? storage object-id)
						(hash-set! storage object-id (cons #f object))
					)

					(let ((include? (car (hash-ref storage object-id))) (distance (objects-distance me object)))
						(cond
							((and (not include?) (<= distance radius))
								(hash-set! storage object-id (cons #t object))
								(trigger-event connection name (list
									(cons 'object object)
									(cons 'action 'come)
								))
							)
							((and include? (> distance radius))
								(hash-set! storage object-id (cons #f object))
								(trigger-event connection name (list
									(cons 'object object)
									(cons 'action 'leave)
								))
							)
						)
					)
				)
			)

			(define (handle-missed object)
				(trigger-event connection name (list
					(cons 'object object)
					(cons 'action 'leave)
				))
			)

			(define (get-missed pairs)
				(let ((ids (lset-difference = (map car pairs) (hash-keys storage))))
					(map (lambda (object-id) (cdr (hash-ref storage object-id))) ids)
				)
			)

			(define (predicate k v)
				(and (integer? k) (not (= k (@: me 'object-id))) (suitable? v))
			)

			(define (checker event)
				(when (equal? temp-interval-event (@: event 'name)) ; check if it's my interval event
					(let ((pairs (hash-filter world predicate)))
						(map handle-new-or-known pairs)
						(map handle-missed (get-missed pairs))
					)
				)

				#f ; always false because events triggered by "run-event"
			)

			(set-interval! connection temp-interval-event sampling-frequency)
			(listen-event! connection name checker)
		)
	)
)
