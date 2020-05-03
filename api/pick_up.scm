(module logic racket/base
	(require
		"../packet/game/client/action.scm"
		"../system/structure.scm"
		"../system/connection.scm"
		"../model/creature.scm"
		"../model/item.scm"
		"../model/world.scm"
	)
	(provide pick-up)

	(define (pick-up cn object-id [shift? #f]) ; TODO what is origin really means?
		(let* ((wr (connection-world cn)) (object (object-ref wr object-id)))
			(when (and object (on-ground? object))
				(send-packet cn (game-client-packet/action object-id (ref object 'position) shift?))
			)
		)
	)
)
