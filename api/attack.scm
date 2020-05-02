(module logic racket/base
	(require
		"../packet/game/client/attack_request.scm"
		"../model/object.scm"
		"../model/creature.scm"
		"../model/world.scm"
		"../system/structure.scm"
		"../system/connection.scm"
	)
	(provide attack)

	(define (attack cn [shift? #f]) ; TODO what is origin really means?
		(let* ((wr (connection-world cn)) (trg (get-target wr (world-me wr))))
			(when (and (creature? trg) (get-position trg))
				(send-packet cn (game-client-packet/attack-request (object-id trg) (get-position trg) shift?))
			)
		)
	)
)
