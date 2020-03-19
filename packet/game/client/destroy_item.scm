; l2j/gameserver/clientpackets/RequestDestroyItem.java
(module system racket/base
	(require "../../packet.scm")
	(provide game-client-packet/destroy-item)

	(define (game-client-packet/destroy-item object-id count)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x59 s)
				(write-int32 object-id #f s)
				(write-int32 count #f s)
				(get-output-bytes s)
			)
		)
	)
)
