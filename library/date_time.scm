(module library racket/base
	(require (rename-in racket/contract (any all/c)))
	(provide (struct-out time-span))
	(provide (contract-out
		(timestamp (-> rational?))
		(time-span->timestamp (-> time-span? rational?))
		; (timestamp->time-span (-> rational? time-span?))
	))

	(struct time-span (days hours minutes seconds) #:transparent)

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
)
