#lang racket
(require srfi/1 "l2apf.scm")

; TODO synchronous logout by contract
; get-event должен возвращать высокоуровневое событие, а управляющие пакеты обрабатывать до передачи управления

(define (find-character name characters)
	(find (lambda (c) (equal? (cdr (assoc 'name c)) "test")) characters)
)

(let ((connection (connect "127.0.0.1")))
	(let ((servers (login connection "test" "123456")))
		(let ((characters (select-server connection (first servers))))
			(let ((world (select-character connection (find-character "test" characters))))
				(display world)
				
				;(let loop ()
				;	(let ((event (get-event connection)))
				;		...
				;		(loop)
				;	)
				;)
			)
		)
	)
)
