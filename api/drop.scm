(module logic racket/base
	(require
		"../packet/game/client/drop_item.scm"
		"../model/creature.scm"
		"../model/world.scm"
		"../system/connection.scm"
	)
	(provide drop)

	(define (drop cn object-id [count 1] [to #f])
		(let ((to (or to (get-position (world-me (connection-world cn))))))
			(and to (send-packet cn (game-client-packet/drop-item object-id count to)))
		)
	)
)
