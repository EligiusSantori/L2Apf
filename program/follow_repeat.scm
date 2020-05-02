(module ai racket/base
	(require
		srfi/1
		racket/undefined
		data/queue
		"program.scm"
		(relative-in "../.."
			"library/extension.scm"
			"system/debug.scm"
			"system/connection.scm"
			"model/map.scm"
			"model/object.scm"
			"model/creature.scm"
			"model/world.scm"
			"api/target.scm"
			"api/move_to.scm"
		)
	)
	(provide program-follow-repeat)

	(define (raise-dont-see-leader object-id)
		(raise-program-error 'program-follow-repeat "Don't see the leader." object-id)
	)

	(define (leader-position cn object-id)
		(let ((creature (object-ref (connection-world cn) object-id)))
			(if creature
				(get-position creature)
				(raise-dont-see-leader object-id)
			)
		)
	)

	(define (go cn point gap)
		(and (>= (points-distance point (get-position (world-me (connection-world cn)))) gap)
			(begin (move-to cn point) #t)
		)
	)

	(define (go-next cn leader-id route gap)
		(if (not (queue-empty? route))
			(or (go cn (dequeue! route) gap) (go-next cn leader-id route gap)) ; Go last or next.
			(let ((point (leader-position cn leader-id)))
				(and (go cn point gap) #f) ; Always not busy.
			)
		)
	)

	(define (follow cn event leader-id gap route busy)
		(cond
			((or (not event) (and (= (first event) (object-id (world-me (connection-world cn)))) (not (third event)))) ; Point reached or first run.
				(go-next cn leader-id route gap)
			)
			((= (first event) leader-id) ; Leader changed moving.
				(enqueue! route (second event)) ; Queue leader position.
				(or busy (go-next cn leader-id route gap)) ; Go next if not busy.
			)
			(else busy)
		)
	)

	(define (make-state route busy)
		(cons route busy)
	)

	(define-program program-follow-repeat
		(lambda (cn event config state)
			(let-values (((leader-id gap) (list->values config)) ((route busy) (car+cdr state)))
				(let ((my-id (object-id (world-me (connection-world cn)))))
					(cond
						((and (eq? (car event) 'change-moving) (member (second event) (list leader-id my-id) =))
							(cons route (follow cn (cdr event) leader-id gap route busy))
						)
						((and (eq? (car event) 'object-delete) (= (second event) leader-id))
							(raise-dont-see-leader leader-id)
						)
						(else state)
					)
				)
			)
		)

		#:constructor (lambda (cn config)
			(let-values (((leader-id gap) (list->values config)))
				(let ((route (make-queue)) (point (leader-position cn leader-id)))
					(target cn leader-id)
					(enqueue! route point)
					(make-state route (follow cn #f leader-id gap route #f))
				)
			)
		)

		#:defaults (list
			undefined ; leader-id (required)
			50 ; gap ; TODO add (or (ref leader 'collision-radius) 0)
		)
	)
)
