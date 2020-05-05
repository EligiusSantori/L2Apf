(module library racket/base
	(require
		racket/string
		racket/format
		racket/contract
	)
	(provide (struct-out time-span))
	(provide (contract-out
		(timestamp (-> rational?))
		(time-span->timestamp (-> time-span? rational?))
		; (timestamp->time-span (-> rational? time-span?))
		(format-time (->* () (rational?) string?))
	))

	(struct time-span (days hours minutes seconds ) #:transparent)

	(define (timestamp)
		(/ (current-inexact-milliseconds) 1000)
	)

	(define (time-span->timestamp ts)
		(+
			(* (time-span-days ts) 86400)
			(* (time-span-hours ts) 3600)
			(* (time-span-minutes ts) 60)
			(time-span-seconds ts)
		)
	)

	(define (format-time [ts (timestamp)])
		(string-append (string-join (map (lambda (v) (~a v #:width 2 #:align 'right #:left-pad-string "0")) (list
			(inexact->exact (floor (/ (remainder (floor ts) 86400) 3600)))
			(inexact->exact (remainder (floor (/ ts 60)) 60))
			(inexact->exact (remainder (floor ts) 60))
		)) ":") "." (~a (inexact->exact (floor (* (- ts (floor ts)) 1000))) #:width 3 #:align 'right #:left-pad-string "0"))
	)
)
