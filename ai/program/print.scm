(module ai racket/base
	(require
		srfi/1
		racket/string
		"program.scm"
	)
	(provide
		ai-program-print ; Redirect game chat to console
	)

	(define (format-chat-message object-id channel author text)
		(let ((channel (string-titlecase (last (string-split (symbol->string channel) "/")))))
			(string-append "[" channel "] " author ": " text)
		)
	)

	(define ai-program-print (struct-copy ai-program ai-program-base
		[id 'print]
		[iterator (lambda (event connection config state)
			(when (and event (eq? (car event) 'message))
				(let-values (((author-id channel author text) (apply values (cdr event))))
					(displayln (format-chat-message author-id channel author text))
				)
			)
			(void)
		)]
	))
)
