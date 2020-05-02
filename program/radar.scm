(module ai racket/base
	(require
		srfi/1
		racket/undefined
		racket/set
		"program.scm"
		(relative-in "../."
			"library/extension.scm"
			"system/connection.scm"
			"system/event.scm"
			"model/object.scm"
			"model/creature.scm"
			"model/map.scm"
			"model/world.scm"
		)
	)
	(provide program-radar)

	(define (match wr radius match?)
		(let ((l (list)) (p (get-position (world-me wr))))
			(hash-for-each (world-objects wr) (lambda (k v)
				(when (and (match? v) (<= (points-distance p (get-position v)) radius))
					(set! l (cons (object-id v) l))
				)
			))
			(list->seteq l)
		)
	)

	(define (pulse cn config last)
		(let-values (((event match? radius interval) (list->values config)))
			(let ((matches (match (connection-world cn) radius match?)))
				(trigger! cn (make-event event
					(set->list matches) ; Current objects.
					(set->list (set-subtract matches last)) ; New objects.
					(set->list (set-subtract last matches)) ; Lost objects.
				))
				matches
			)
		)
	)

	(define-program program-radar
		(lambda (cn event config state)
			(when (eq? (car event) (car state)) ; If timer event.
				(cons (car state) (pulse cn config (cdr state)))
			)
		)

		#:constructor (lambda (cn config)
			(let ((timer (interval! cn (fourth config))))
				(cons timer (pulse cn config (seteq)))
			)
		)

		#:defaults (list
			undefined ; event (required)
			object? ; predicate
			10000 ; radius
			1000 ; interval
		)
	)
)
