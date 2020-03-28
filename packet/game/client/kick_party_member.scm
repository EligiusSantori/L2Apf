; l2j/gameserver/clientpackets/RequestOustPartyMember.java
(module system racket/base
	(require "../../packet.scm")
	(provide game-client-packet/kick-party-memeber)

	(define (game-client-packet/kick-party-memeber name)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x2c s)
				(write-utf16 name s)
				(get-output-bytes s)
			)
		)
	)
)
