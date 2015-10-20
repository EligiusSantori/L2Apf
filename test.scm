#lang racket
(require
	srfi/1
	"library/extension.scm"
	"library/structure.scm"
	"logic/object.scm"
	"logic/antagonist.scm"
	;(only-in "system/events.scm" listen-event! trigger-event)
	;(only-in "system/timers.scm" set-timeout! set-interval!)
	"api/connect.scm"
	"api/login.scm"
	"api/select_server.scm"
	"api/select_character.scm"
	"api/gesture.scm"
	"api/move_to.scm"
	"api/move_on.scm"
	"api/logout.scm"
	"system/contract.scm"
	"system/uri_scheme.scm"
	"event/radar.scm"
)

(define (parse-config command-line)
	(if (> (vector-length command-line) 0)
		(let ((uri (parse-uri (vector-ref command-line 0))))
			(if uri
				(apply values uri)
				(error "Authentication failed because URI is broken")
			)
		)
		(error "Authentication failed because URI is missed")
	)
)

(define (format-chat-message object-id channel author text)
	(let ((channel (string-titlecase (last (string-split (symbol->string channel) "/")))))
		(string-append "[" channel "] " author ": " text)
	)
)

(let-values (((account password host port name) (parse-config (current-command-line-arguments))))
	(let ((connection (connect host port)))
		(let ((world (first (login connection account password))))
			(let ((me (@: (select-server connection world) name)))
				(let ((events (select-character connection me)))
					
					(define move-on/sync (make-contract move-on (lambda (event)
						(and
							(equal? (first event) 'change-moving)
							(equal? (second event) (@: me 'object-id))
							(not (@: me 'moving?))
							
						)
					)))
					
					(define (turn-to-face/sync connection to)
						(move-on/sync connection 25 (objects-angle me to) #f)
						(sleep 1/3) ; overspeed interaction fix
					)
					
					;(set-radar-event! connection 'he-so-close 250)
					
					(let loop ()
						(let ((event (sync events)))
							(case (if event (car event) #f)
								; custom events
								#|
								((he-so-close)
									(let ((object (@: event 'object)))
										(when (and (antagonist? object) (string-ci=? (@: object 'name) "Awe"))
											(turn-to-face/sync connection object)
											(case (@: event 'action) ; greetings and goodbye
												((come) (gesture connection 'gesture/hello))
												((leave) (gesture connection 'gesture/bow))
											)
										)
									)
								)
								|#
								; standard events
								((message) (let-values (((object-id channel author text) (apply values (cdr event))))
									(displayln (format-chat-message object-id channel author text))
								))
								((logout)
									(exit)
								)
							)
						)
						(loop)
					)
				)
			)
		)
	)
)
