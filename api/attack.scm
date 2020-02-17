(module logic racket/base
	(require
		"../packet/game/client/attack_request.scm"
		"../model/creature.scm"
		"../model/world.scm"
		"../system/structure.scm"
		"../system/connection.scm"
	)
	(provide attack)

	(define (attack cn [shift? #f])
		(let* ((wr (connection-world cn))
					(object-id (ref (world-me wr) 'target-id))
						(object (object-ref world object-id))
							(origin (ref object 'position))) ; TODO what is origin really means?
			(when (and (creature? object) origin)
				(send-packet cn (game-client-packet/attack-request object-id origin shift?))
			)
		)
	)
)
