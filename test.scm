#lang racket
(require
	srfi/1
	"library/extension.scm"
	"library/structure.scm"
	"library/logic.scm"
	;(only-in "system/events.scm" set-event! run-event)
	;(only-in "system/timers.scm" set-timeout! set-interval!)
	"api/connect.scm"
	"api/login.scm"
	"api/select_server.scm"
	"api/select_character.scm"
	"api/logout.scm"
	"api/social_action.scm"
	"api/move_to.scm"
	"api/move_on.scm"
	"event/radar.scm"
	"l2apf.scm"
)

; TODO command-line arguments
(define config/host "localhost")
(define config/name "test")
(define config/login "test")
(define config/password "123456")
(define config/admin "admin")


(define (format-chat-message e)
	(let ((channel (symbol->string (@: e 'channel))))
		(string-append
			"[" (string-titlecase (last (string-split channel "/"))) "] "
			(@: e 'author) ": " (@: e 'text)
		)
	)
)

(let ((connection (connect config/host)))
	(let ((world (first (login connection config/login config/password))))
		(let ((me (@: (select-server connection world) config/name)))
			(let ((events (select-character connection me)))

				(set-radar-event! connection 'he-so-close 1000)
				
				(let loop ()
					(let ((event (sync events)))
						(case (if event (@: event 'name) #f)
							; custom events
							((he-so-close)
								(let ((object (@: event 'object)))
									(when (and (antagonist? object) (equal? (@: object 'name) config/admin))
										(move-on connection 10 (objects-angle me object) #t) ; turn toward
										(case (@: event 'action) ; greetings and goodbye
											((come) (social-action connection 'social-action/hello))
											((leave) (social-action connection 'social-action/bow))
										)
									)
								)
							)
							; standard events
							((message)
								(displayln (format-chat-message event))
							)
							((unhandled-packet)
								(displayln (format "unhandled packet #~x" (@: event 'id)))
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
