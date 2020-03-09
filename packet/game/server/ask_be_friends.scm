(module system racket/base
	(require racket/string "../../packet.scm")
	(provide game-server-packet/ask-be-friends)

	(define (game-server-packet/ask-be-friends buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'name (string-trim (read-utf16 s)))
			)
		)
	)
)
