(module ai racket/base
	(require
		srfi/1
		racket/math
		racket/undefined
		"program.scm"
		(relative-in "../."
			"library/extension.scm"
			"system/structure.scm"
			"system/connection.scm"
			"model/map.scm"
			"model/creature.scm"
			"model/world.scm"
			"api/target.scm"
			"api/move_to.scm"
		)
	)
	(provide program-follow-chase)

	(define (raise-dont-see-leader object-id)
		(raise-program-error 'program-follow-chase "Don't see the leader." object-id)
	)

	(define (get-leader cn leader-id)
		(let ((leader (object-ref (connection-world cn) leader-id)))
			(or leader (raise-dont-see-leader leader-id))
		)
	)

	(define (follow cn leader margin)
		(move-to cn
			(or (get-destination (connection-world cn) leader) (get-position leader))
			(exact-round (+ margin (or (ref leader 'collision-radius) 0)))
		)
		(void)
	)

	(define-program program-follow-chase
		(lambda (cn event config state)
			(let-values (((leader-id margin) (list->values config)))
				(cond
					((and (eq? (car event) 'change-moving) (= (second event) leader-id))
						(follow cn (get-leader cn leader-id) margin)
					)
					((and (eq? (car event) 'object-delete) (= (second event) leader-id))
						(raise-dont-see-leader leader-id)
					)
					(else (void))
				)
			)
		)

		#:constructor (lambda (cn config)
			(let-values (((leader-id margin) (list->values config)))
				(let ((leader (get-leader cn leader-id)))
					(target cn leader-id)
					(follow cn leader margin)
				)
			)
		)

		#:defaults (list
			undefined ; leader-id (required)
			30 ; margin
		)
	)
)
