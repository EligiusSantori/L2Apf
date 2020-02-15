(module logic racket/base
	(require
		"../system/structure.scm"
		"../system/network.scm"
		"../model/creature.scm"
		"../model/world.scm"
		"../packet/game/client/attack_request.scm"
	)
	(provide attack)

	(define (attack connection [shift? #f])
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
