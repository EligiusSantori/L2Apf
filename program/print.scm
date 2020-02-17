(module ai racket/base
	(require
		srfi/1
		racket/string
		racket/undefined
		"program.scm"
	)
	(provide
		program-print ; Redirect game chat to console
	)

	(define (format-chat-message object-id channel author text)
		(let ((channel (string-titlecase (last (string-split (symbol->string channel) "/")))))
			(string-append "[" channel "] " author ": " text)
		)
	)

	(define-program program-print undefined undefined undefined
		(lambda (event connection . args)
			(when (eq? (car event) 'message)
				(let-values (((author-id channel author text) (apply values (cdr event))))
					(displayln (format-chat-message author-id channel author text))
				)
			)
			(void)
		)
	)
)
