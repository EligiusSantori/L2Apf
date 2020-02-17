(module logic racket/base
	(require
		"../packet/game/client/action.scm"
		"../model/creature.scm"
		"../model/world.scm"
		"../system/structure.scm"
		"../system/connection.scm"
	)
	(provide target)

	(define (target cn object-id [shift? #f] [control? #f])
		(let* ((wr (connection-world cn)) (object (object-ref wr object-id)))
			(when (and (or control? (not (equal? object-id (ref (world-me wr) 'target-id)))) (creature? object))
				(send-packet cn (game-client-packet/action object-id (ref object 'position) shift?)) ; TODO what is origin really means?
			)
		)
	)
)
