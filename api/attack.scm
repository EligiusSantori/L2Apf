(module api racket/base
	(require
		"../library/structure.scm"
		"../library/network.scm"
		"../logic/creature.scm"
		"../logic/world.scm"
		"../packet/game/client/attack_request.scm"
	)
	(provide attack)
	
	(define (attack connection . tail)
		(define shift? (if (null? tail) #f (car tail)))
		
		(let* ((world (@: connection 'world))
					(object-id (@: world 'me 'target-id))
						(object (object-ref world object-id))
							(origin (@: object 'position))) ; TODO what is origin really means?
			(when (and (creature? object) origin)
				(send connection (game-client-packet/attack-request object-id origin shift?))
			)
		)
	)
)