(module script racket/base
	(require
		srfi/1
		racket/string
		(relative-in "../.."
			"system/uri_scheme.scm"
			"library/extension.scm"
		)
	)
	(provide
		parse-protocol
		parse-command
		format-chat-message
	)
	
	(define (parse-protocol command-line)
		(if (> (vector-length command-line) 0)
			(let ((uri (parse-uri (vector-ref command-line 0))))
				(if uri
					(apply values uri)
					(error "Authentication failed because URI is broken")
				)
			)
			(error "Authentication failed because URI is missed")
		)
	)

	(define (parse-command text)
		(let ((t (string-trim (string-downcase text))))
			(if (string-starts? t "/")
				(let ((l (map string-trim (string-split t " "))))
					(cons (substring (car l) 1) (cdr l))
				)
				#f
			)
		)
	)

	(define (format-chat-message object-id channel author text)
		(let ((channel (string-titlecase (last (string-split (symbol->string channel) "/")))))
			(string-append "[" channel "] " author ": " text)
		)
	)
)
