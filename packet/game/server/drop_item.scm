; l2j/gameserver/serverpackets/DropItem.java
(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/drop-item)

	(define (game-server-packet/drop-item buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'subject-id (read-int32 #f s))
				(cons 'object-id (read-int32 #f s))
				(cons 'item-id (read-int32 #f s))
				(cons 'position (read-point s))
				(cons 'stackable? (not (zero? (read-int32 #f s))))
				(cons 'count (read-int32 #f s))
			)
		)
	)
)
