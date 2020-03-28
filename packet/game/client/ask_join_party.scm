; l2j/gameserver/clientpackets/RequestJoinParty.java
(module system racket/base
	(require
		"../../../library/extension.scm"
		"../../packet.scm"
		"../party_loot.scm"
	)
	(provide game-client-packet/ask-join-party)

	(define (game-client-packet/ask-join-party name loot)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x29 s)
				(write-utf16 name s)
				(write-int32 (cdr (assoc loot (alist-flip loot-types))) #f s)
				(get-output-bytes s)
			)
		)
	)
)
