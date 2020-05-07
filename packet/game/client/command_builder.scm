; l2j/gameserver/clientpackets/SendBypassBuildCmd.java
(module system racket/base
	(require
		"../../packet.scm"
	)
	(provide game-client-packet/command-builder)

	(define (game-client-packet/command-builder command)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x5b s)
				(write-utf16 command s)
				(get-output-bytes s)
			)
		)
	)
)
