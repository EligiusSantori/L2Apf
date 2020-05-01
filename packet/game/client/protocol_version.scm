(module system racket/base
	(provide game-client-packet/protocol-version)
	(require "../../packet.scm")

	(define (game-client-packet/protocol-version struct)
		(let ((s (open-output-bytes)))
			(begin
				(write-byte #x00 s)
				(write-int32 (cdr (assoc 'protocol struct)) #f s)
				(write-bytes (bytes ; nkey[256]
					#x09 #x07 #x54 #x56 #x03 #x09 #x0B #x01 #x07 #x02 #x54 #x54 #x56 #x07 #x00 #x02
					#x55 #x56 #x00 #x51 #x00 #x53 #x57 #x04 #x07 #x55 #x08 #x54 #x01 #x07 #x01 #x53
					#x00 #x56 #x55 #x56 #x01 #x06 #x05 #x04 #x51 #x03 #x08 #x51 #x08 #x51 #x56 #x04
					#x54 #x06 #x55 #x08 #x02 #x09 #x51 #x56 #x01 #x53 #x06 #x55 #x04 #x53 #x00 #x56
					#x56 #x53 #x01 #x09 #x02 #x09 #x01 #x51 #x54 #x51 #x09 #x55 #x56 #x09 #x03 #x04
					#x07 #x05 #x55 #x04 #x06 #x55 #x04 #x06 #x09 #x04 #x51 #x01 #x08 #x08 #x06 #x05
					#x52 #x06 #x04 #x01 #x07 #x54 #x03 #x06 #x52 #x55 #x06 #x55 #x55 #x51 #x01 #x02
					#x04 #x54 #x03 #x55 #x54 #x01 #x57 #x51 #x55 #x05 #x52 #x05 #x54 #x07 #x51 #x51
					#x55 #x07 #x02 #x53 #x53 #x00 #x52 #x05 #x52 #x07 #x01 #x54 #x00 #x03 #x05 #x05
					#x08 #x06 #x05 #x05 #x06 #x03 #x00 #x0D #x08 #x01 #x07 #x09 #x03 #x51 #x03 #x07
					#x53 #x09 #x51 #x06 #x07 #x54 #x0A #x50 #x56 #x02 #x52 #x04 #x05 #x55 #x51 #x02
					#x53 #x00 #x08 #x54 #x04 #x52 #x56 #x06 #x02 #x09 #x00 #x08 #x03 #x53 #x56 #x01
					#x05 #x00 #x55 #x06 #x08 #x56 #x04 #x0D #x06 #x07 #x52 #x06 #x07 #x04 #x0A #x06
					#x01 #x04 #x54 #x04 #x00 #x05 #x02 #x04 #x54 #x00 #x09 #x52 #x53 #x05 #x04 #x01
					#x04 #x05 #x05 #x01 #x52 #x51 #x52 #x0D #x06 #x51 #x08 #x09 #x54 #x53 #x00 #x0D
					#x01 #x02 #x03 #x54 #x53 #x01 #x05 #x03 #x08 #x56 #x54 #x07 #x02 #x54 #x0B #x06
				) s)
				(get-output-bytes s)
			)
		)
	)
)
