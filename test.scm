#lang racket
(require srfi/1 "l2apf.scm")

(define channel #f)
(define world #f)
(define me #f)

(define (find-character name characters)
	(find (lambda (c) (equal? (cdr (assoc 'name c)) "test")) characters)
)

(let ((connection (connect "127.0.0.1")))
	(let ((servers (login connection "test" "123456")))
		(let ((characters (select-server connection (first servers))))
			(let ((events (select-character connection (find-character "test" characters))))
			
				(set-interval connection 'party-time 30000)
				
				(let loop ()
					(let ((event (sync events)))
						(case (cdr (assoc 'name event))
							((party-time)
								(displayln "It's party time!") ; TODO (social-action 'social-action/dance)
							)
							((message)
								
							)
						)
						(loop)
					)
				)
			)
		)
	)
)
