(module ai racket/base
	(require
		srfi/1
		racket/string
		racket/undefined
		"program.scm"
		"../system/event.scm"
	)
	(provide make-program-print) ; Redirect game chat to console

	(define (format-chat-message object-id channel author text)
		(let ((channel (string-titlecase (last (string-split (symbol->string channel) "/")))))
			(string-append "[" channel "] " author ": " text)
		)
	)

	(define (make-program-print)
		(make-program 'program-print
			(lambda (cn event . args)
				(case-event event
					('message (author-id channel author text)
						(displayln (format-chat-message author-id channel author text))
					)
				)
				(void)
			)
		)
	)
)
