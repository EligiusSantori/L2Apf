; l2j/gameserver/clientpackets/RequestChangePartyLeader.java
(module system racket/base
	(require "../../packet.scm")
	(provide game-client-packet/change-party-leader)

	(define (game-client-packet/change-party-leader name)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #xd0 s)
				(write-int16 #x04 #f s)
				(write-utf16 name s)
				(get-output-bytes s)
			)
		)
	)
)
