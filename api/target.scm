(module api racket/base
	(require
		"../library/structure.scm"
		"../library/network.scm"
		"../model/creature.scm"
		"../model/world.scm"
		"../packet/game/client/action.scm"
	)
	(provide target)
	
	(define (target connection object-id [shift? #f])
		; TODO check if me.target.id != target-id
		(let* ((world (@: connection 'world)) (object (object-ref world object-id)))
			(when (creature? object) ; TODO what is origin really means?
				(send connection (game-client-packet/action object-id (@: object 'position) shift?))
			)
		)
	)
)