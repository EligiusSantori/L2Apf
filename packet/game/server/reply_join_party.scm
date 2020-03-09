; l2j/gameserver/serverpackets/JoinParty.java
(module system racket/base
	(require "../../packet.scm")
	(provide game-server-packet/reply-join-party)

	(define (game-server-packet/reply-join-party buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'accept? (not (zero? (read-int32 #f s))))
			)
		)
	)
)
