(module ai racket/base
	(require
		srfi/1
		racket/math
		racket/undefined
		"program.scm"
		(relative-in "../."
			"library/extension.scm"
			"system/debug.scm"
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

	(define (follow cn leader-id margin)
		(let ((leader (object-ref (connection-world cn) leader-id)))
			(if leader
				(move-to cn
					(or (ref leader 'destination) (get-position leader))
					(exact-round (+ margin (or (ref leader 'collision-radius) 0)))
				)
				(apf-warn "Don't see the leader ~v, program: follow-chase." leader-id)
			)
		)
	)

	(define-program program-follow-chase
		(list
			undefined ; leader-id (required)
			30 ; margin
		)
		(lambda (cn config)
			(let-values (((leader-id margin) (list->values config)))
				(target cn leader-id)
				(follow cn leader-id margin)
			)
			(void)
		)
		undefined
		(lambda (cn event config state)
			(let-values (((leader-id margin) (list->values config)))
				(when (and (eq? (car event) 'change-moving) (= (second event) leader-id))
					(follow cn leader-id margin)
				)
			)
			(void)
		)
	)
)
