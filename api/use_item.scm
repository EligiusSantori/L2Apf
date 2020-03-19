(module logic racket/base
	(require
		"../packet/game/client/use_item.scm"
		"../packet/game/client/unequip_slot.scm"
		"../model/world.scm"
		"../system/connection.scm"
	)
	(provide use-item)

	(define (use-item cn object-id)
		(let ((item (inv-ref (connection-world cn) object-id))) (when item
			(let ((slot (equipped (connection-world cn) item)))
				(if (not slot)
					(send-packet cn (game-client-packet/use-item object-id #f))
					(send-packet cn (game-client-packet/unequip-slot slot))
				)
			)
		))
	)
)
