; l2j/gameserver/serverpackets/GetItem.java
(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/pick-item)

	(define (game-server-packet/pick-item buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'subject-id (read-int32 #f s))
				(cons 'object-id (read-int32 #f s))
				(cons 'position (read-point s))
			)
		)
	)
)
