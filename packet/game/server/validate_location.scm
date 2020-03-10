; l2j/gameserver/serverpackets/ValidateLocation.java
(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/validate-location)

	(define (game-server-packet/validate-location buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
				(cons 'position (read-point s))
				(cons 'angle (heading->angle (read-int32 #f s)))
			)
		)
	)
)
