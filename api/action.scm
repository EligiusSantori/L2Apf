; l2j/gameserver/network/clientpackets/Action.java
(module logic racket/base
	(require
		"../packet/game/client/action.scm"
		"../model/creature.scm"
		"../model/world.scm"
		"../system/structure.scm"
		"../system/connection.scm"
	)
	(provide action)

	(define (action cn [shift? #f])
		(let* ((target (get-target wr (world-me (connection-world cn)))) (origin (get-position target)))
			(when (and (creature? target) origin) ; TODO what is origin really means?
				(send-packet cn (game-client-packet/action (object-id target) origin shift?))
			)
		)
	)
)
