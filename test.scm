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

(define (get-config command-line)
	(if (> (vector-length command-line) 0)
		(or
			(parse-uri (vector-ref command-line 0))
			(error "Authentication failed because URI is broken")
		)
		(error "Authentication failed because URI is missed")
	)
)

(define (format-chat-message e)
	(let ((channel (symbol->string (@: e 'channel))))
		(string-append
			"[" (string-titlecase (last (string-split channel "/"))) "] "
			(@: e 'author) ": " (@: e 'text)
		)
	)
)

(let ((config (get-config (current-command-line-arguments))))
	(let ((connection (connect (@: config 'host) (@: config 'port)))) ; TODO or die?
		(let ((world (first (login connection (@: config 'login) (@: config 'password))))) ; TODO or die?
			(let ((me (@: (select-server connection world) (@: config 'name)))) ; TODO or die?
				(let ((events (select-character connection me)))

					(define move-on/sync (make-contract move-on (lambda (event)
						(and
							(equal? (@: event 'name) 'change-moving)
							(equal? (@: event 'action) 'stop)
							(equal? (@: event 'object-id) (@: me 'object-id))
						)
					)))
					
					(define (turn-to-face/sync connection to)
						(move-on/sync connection 25 (objects-angle me to) #f)
						(sleep 1/3) ; overspeed interaction fix
					)
					
					(set-radar-event! connection 'he-so-close 250)
					
					(let loop ()
						(let ((event (sync events)))
							(case (if event (@: event 'name) #f)
								; custom events
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
								; standard events
								((message)
									(displayln (format-chat-message event))
								)
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