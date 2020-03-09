(module system racket/base
	(require racket/string "../../packet.scm")
	(provide game-server-packet/ask-join-alliance)

	(define (game-server-packet/ask-join-alliance buffer)
		(let ((s (open-input-bytes buffer)))
			(list
				(cons 'id (read-byte s))
				(cons 'object-id (read-int32 #f s))
				(cons 'name (string-trim (read-utf16 s)))
			)
		)
	)
)
