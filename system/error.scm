(module system racket/base
	(require racket/contract)
	(provide (contract-out
		(error-format (->* (string?) #:rest list? string?))
	))

	(define (error-format message . args)
		; (let ((ph (error-value->string-handler)) (pw (error-print-width)))
		; 	(string-join (cons message (map (lambda (arg) (ph arg pw)) args)) " ")
		; )

		(let ((o (open-output-string))) ; Better compatibility with raise-user-error.
			(display message o)
			(map (lambda (arg) (display " " o) (display arg o)) args)
			(get-output-string o)
		)
	)
)
