; TODO mental-shield не всегда срабатывает почему-то и проблема на сервере

(module script racket/base
	(require
		(relative-in "../.."
			"library/structure.scm"
			"model/world.scm"
			"api/use_skill.scm"
			"api/target_sync.scm"
		)
	)
	(provide make-buffer)

	(define (filter-todo todo)
		(and todo
			(filter (lambda (task)
				(and task (> (length task) 1))
			) (map (lambda (task)
				(filter integer? task)
			) todo))
		)
	)

	(define (make-buffer connection)
		(define todo (list))
		(define queue (list))

		(define (iterate)
			(if (or (not (null? todo)) (not (null? queue)))
				(begin
					(when (null? queue) ; Switch between players and fill queue from todo
						(let ((task (car todo)))
							(set! todo (cdr todo))
							(set! queue (cdr task))
							(target/sync connection (car task))
						)
					)
					(let ((skill-id (car queue))) ; Process queue
						(set! queue (cdr queue))
						(if (skill-ref (@: connection 'world) skill-id) ; If skill exists
							(begin (use-skill connection skill-id) #t) ; Buff and yield
							(iterate) ; Skip to next
						)
					)
				)
				#f
			)
		)

		(lambda arguments
			(if (not (null? arguments)) ; New to do
				(let ((argument (filter-todo (car arguments))))
					(and argument (not (null? argument)) (begin
							(set! todo argument)
							(set! queue (list))
							(iterate)
					))
				)
				(iterate) ; Dequeue buff
			)
		)
	)
)
