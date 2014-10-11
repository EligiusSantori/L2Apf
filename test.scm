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
	"system/contract.scm"
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

				(set-radar-event! connection 'he-so-close 250)
				(define move-on/sync (make-contract move-on (lambda (event)
					(and
						(equal? (@: event 'name) 'change-moving)
						(equal? (@: event 'action) 'stop)
						(equal? (@: event 'object-id) (@: me 'object-id))
						(sleep 1/3) ; overspeed interaction fix
					)
				)))
				
				(let loop ()
					(let ((event (sync events)))
						(case (if event (@: event 'name) #f)
							; custom events
							((he-so-close)
								(let ((object (@: event 'object)))
									(when (and (antagonist? object) (string-ci=? (@: object 'name) config/admin))
										(move-on/sync connection 25 (objects-angle me object) #f) ; turn toward
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
