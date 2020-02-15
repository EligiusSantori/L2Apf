(module logic racket/base
	(require
		"../system/structure.scm"
		"../system/network.scm"
		"../model/creature.scm"
		"../model/world.scm"
		"../packet/game/client/action.scm"
	)
	(provide target)

	(define (target connection object-id [shift? #f] [control? #f])
		(let* ((world (@: connection 'world)) (object (object-ref world object-id)))
			(when (and (or control? (not (equal? object-id (@: world 'me 'target-id)))) (creature? object))
				(send connection (game-client-packet/action object-id (@: object 'position) shift?)) ; TODO what is origin really means?
			)
		)
	)
)
