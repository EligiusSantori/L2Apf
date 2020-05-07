(module system racket/base
	(require
		racket/contract
		"../library/date_time.scm"
	)
	(provide (contract-out
		(apf-fatal (->* (string?) #:rest list? void?))
		(apf-error (->* (string?) #:rest list? void?))
		(apf-warn (->* (string?) #:rest list? void?))
		(apf-info (->* (string?) #:rest list? void?))
		(apf-debug (->* (string?) #:rest list? void?))

	))

	(define (apf-message level pattern . values)
		(let ((logger (current-logger)))
			(when (log-level? logger level)
				(log-message logger level 'l2apf (string-append (format-time) ": "
					(if (null? values) pattern (apply format pattern values))
				) #f #f)
			)
		)
	)
	(define (apf-fatal pattern . values)
		(apply apf-message 'fatal pattern values)
	)
	(define (apf-error pattern . values)
		(apply apf-message 'error pattern values)
	)
	(define (apf-warn pattern . values)
		(apply apf-message 'warning pattern values)
	)
	(define (apf-info pattern . values)
		(apply apf-message 'info pattern values)
	)
	(define (apf-debug pattern . values)
		(apply apf-message 'debug pattern values)
	)
)
