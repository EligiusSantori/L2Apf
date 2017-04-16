(module api racket/base
	(require
		(rename-in racket/contract (any all/c))
		"../library/structure.scm"
		"../library/geometry.scm"
		"../library/network.scm"
		"../packet/game/client/move_to_point.scm"
	)
	(provide (contract-out 
		(move-to (->* (connection? point/3d?) (integer?) boolean?))
	))
	
	(define (move-to connection point [gap 0])
		(let* ((from (@: connection 'world 'me 'position)) (distance (distance/3d from point)))		
			(and (> distance gap)
				(send connection (game-client-packet/move-to-point from (if (> gap 0)
					(segment-offset/3d point from gap)
					point
				)))
				#t
			)
		)
	)
)
