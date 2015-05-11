#lang racket
(require
	srfi/1
	"library/extension.scm"
	"library/structure.scm"
	"logic/object.scm"
	"logic/antagonist.scm"
	;(only-in "system/events.scm" set-event! run-event)
	;(only-in "system/timers.scm" set-timeout! set-interval!)
	"api/connect.scm"
	"api/login.scm"
	"api/select_server.scm"
	"api/select_character.scm"
	"api/social_action.scm"
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
	(let ((connection (connect (@: config 'host) (@: config 'port))))
		(let ((world (first (login connection (@: config 'login) (@: config 'password)))))
			(let ((me (@: (select-server connection world) (@: config 'name))))
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
										(when (and (antagonist? object) (string-ci=? (@: object 'name) "Awe"))
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
)