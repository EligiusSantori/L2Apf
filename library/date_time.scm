(struct time-span (days hours minutes seconds) #:transparent)

(define (time-span->milliseconds ts)
	(* 1000 (+
		(* (time-span-days ts) 86400)
		(* (time-span-hours ts) 3600)
		(* (time-span-minutes ts) 60)
		(time-span-seconds ts)
	))
)
