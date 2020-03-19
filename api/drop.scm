(module logic racket/base
	(require
		"../packet/game/client/destroy_item.scm"
		"../packet/game/client/crystallize_item.scm"
		"../packet/game/client/drop_item.scm"
		"../model/creature.scm"
		"../model/world.scm"
		"../system/connection.scm"
	)
	(provide drop)

	(define (drop cn object-id [count 1] [toa #f])
		(if (symbol? toa)
			(case toa
				((destroy) (send-packet cn (game-client-packet/destroy-item object-id count)))
				((crystallize) (send-packet cn (game-client-packet/crystallize-item object-id count)))
			)
			(let ((to (or toa (get-position (world-me (connection-world cn))))))
				(and to (send-packet cn (game-client-packet/drop-item object-id count to)))
			)
		)
	)
)
