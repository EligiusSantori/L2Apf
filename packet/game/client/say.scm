; l2j/gameserver/clientpackets/Say2.java
(module system racket/base
	(require
		"../../../library/extension.scm"
		"../../packet.scm"
		"../channel.scm"
	)
	(provide game-client-packet/say)

	(define (game-client-packet/say message channel name)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x38 s)
				(write-utf16 message s)
				(write-int32 (cdr (assoc channel (alist-flip channels))) #t s)
				(when (and (equal? channel 'chat-channel/tell) (not (string=? name "")))
					(write-utf16 name s)
				)
				(get-output-bytes s)
			)
		)
	)
)
