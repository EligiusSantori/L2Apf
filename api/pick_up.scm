(module logic racket/base
	(require
		"../system/structure.scm"
		"../system/network.scm"
		"../model/creature.scm"
		"../model/item.scm"
		"../model/world.scm"
		"../packet/game/client/action.scm"
	)
	(provide pick-up)

	(define (pick-up connection object-id [shift? #f])
		(let* ((world (@: connection 'world)) (object (object-ref world object-id)) (origin (@: object 'position))) ; TODO what is origin really means?
			(when (and (on-ground? object) origin)
				(send connection (game-client-packet/action object-id origin shift?))
			)
		)
	)
)
