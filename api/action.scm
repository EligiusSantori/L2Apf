(module api racket/base
	(require
		"../library/structure.scm"
		"../library/network.scm"
		"../model/creature.scm"
		"../model/world.scm"
		"../packet/game/client/action.scm"
	)
	(provide action)
	
	(define (action connection . tail)
		(define shift? (if (null? tail) #f (car tail)))

		(let* ((world (@: connection 'world))
					(object-id (@: world 'me 'target-id))
						(object (object-ref world object-id))
							(origin (@: object 'position))) ; TODO what is origin really means?
			(when (and (creature? object) origin)
				(send connection (game-client-packet/action object-id origin shift?))
			)
		)
	)
)
