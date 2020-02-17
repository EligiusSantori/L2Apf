(module logic racket/base
	(require
		"../packet/game/client/action.scm"
		"../model/creature.scm"
		"../model/world.scm"
		"../system/structure.scm"
		"../system/connection.scm"
	)
	(provide action)

	(define (action cn . tail)
		(define shift? (if (null? tail) #f (car tail)))

		(let* ((wr (connection-world cn))
					(object-id (ref (world-me wr) 'target-id))
						(object (object-ref world object-id))
							(origin (ref object 'position))) ; TODO what is origin really means?
			(when (and (creature? object) origin)
				(send-packet cn (game-client-packet/action object-id origin shift?))
			)
		)
	)
)
