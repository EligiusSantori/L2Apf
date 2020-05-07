(module ai racket/base
	(require
		srfi/1
		racket/undefined
		data/queue
		"program.scm"
		(relative-in "../.."
			"library/extension.scm"
			"system/connection.scm"
			"system/event.scm"
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

	(define (get-leader cn leader-id)
		(let ((creature (object-ref (connection-world cn) leader-id)))
			(or creature (raise-dont-see-leader leader-id))
		)
	)
	(define (go-when-far cn point skip)
		(let ((distance (points-distance (get-position (world-me (connection-world cn))) point)))
; (printf "{go-when-far ~a > ~a}~n" distance skip) (flush-output)
			(and (> distance skip)
				(move-to cn point)
			)
		)
	)
	(define (go-with-gap cn point gap)
; (printf "{go-with-gap ~a}~n" gap) (flush-output)
		(move-to cn point gap)
	)
	(define (go-next cn leader-id route skip)
		(let ((wr (connection-world cn)) (leader (get-leader cn leader-id)))
			(if (not (queue-empty? route))
				(or ; Go to next queue point or over it.
					(go-when-far cn (dequeue! route) skip)
					(go-next cn leader-id route skip)
				)
				(let ((destination (get-destination wr leader)))
					(when destination
						(go-when-far cn destination skip)
					)
					#f ; Always not busy.
				)
			)
		)
	)

	(define (follow cn event leader-id skip route busy)
		(cond
			((or (not event) (and (= (first event) (object-id (world-me (connection-world cn)))) (not (third event)))) ; Point reached or first run.
				(go-next cn leader-id route skip)
			)
			((= (first event) leader-id) ; Leader changed moving.
				(enqueue! route (second event)) ; Queue leader position.
				(or busy (go-next cn leader-id route skip)) ; Go next if not busy.
			)
			(else busy)
		)
	)

	(define (make-state route busy)
		(cons route busy)
	)

	(define-program program-follow-repeat
		(lambda (cn event config state)
			(let-values (((leader-id skip) (list->values config)) ((route busy) (car+cdr state)))
				(let ((my-id (object-id (world-me (connection-world cn)))))
					(cond
						((and (eq? (event-name event) 'change-moving) (member (second event) (list leader-id my-id) =))
							(cons route (follow cn (cdr event) leader-id skip route busy))
						)
						((and (eq? (event-name event) 'object-delete) (= (second event) leader-id))
							(raise-dont-see-leader leader-id)
						)
						(else state)
					)
				)
			)
		)

		#:constructor (lambda (cn config)
			(let-values (((leader-id skip) (list->values config)))
				(let ((route (make-queue)) (point (get-position (get-leader cn leader-id))))
					(target cn leader-id)
					(enqueue! route point)
					(make-state route (follow cn #f leader-id skip route #f))
				)
			)
		)

		#:defaults (list
			undefined ; leader-id (required)
			50 ; skip ; TODO add (or (ref leader 'collision-radius) 0)?
		)
	)
)
