#lang racket
(require srfi/1 "library/extension.scm" "library/structure.scm" "l2apf.scm")

; TODO Параметры командной строки

(define (format-chat-message e)
	(string-append
		"[" (string-titlecase (last (string-split (symbol->string (get-field e 'channel)) "/"))) "] "
		(get-field e 'author) ": " (get-field e 'text)
	)
)

(letone connection (connect "127.0.0.1")
	(letone world (first (login connection "test" "123456"))
		(letone me (get-field (select-server connection world) "test")
			(letone events (select-character connection me)

				(set-interval connection 'party-time 7000)
			
				(let loop ()
					(letone event (sync events)
						(case (get-field event 'name)
							((party-time)
								(social-action connection 'social-action/dance)
							)
							((message)
								(displayln (format-chat-message event))
							)
						)
					)
					(loop)
				)
				
			)
		)
	)
)
