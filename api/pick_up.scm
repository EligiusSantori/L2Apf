(module logic racket/base
	(require
		"../packet/game/client/action.scm"
		"../model/creature.scm"
		"../model/item.scm"
		"../model/world.scm"
		"../system/structure.scm"
		"../system/connection.scm"
	)
	(provide pick-up)

	(define (pick-up cn object-id [shift? #f])
		(let* ((wr (connection-world cn)) (object (object-ref wr object-id)) (origin (ref object 'position))) ; TODO what is origin really means?
			(when (and (on-ground? object) origin)
				(send-packet cn (game-client-packet/action object-id origin shift?))
			)
		)
	)
)
