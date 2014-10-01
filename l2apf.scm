; TODO Сообщения для пакетов ошибок
; TODO Лучше два события со своими регулярными параметрами, чем одно с переключающимися!
; TODO Ключевые пакеты и ключевые события: social-action, use-item, use-skill, move-to, action, target; message, creature-update
; TODO logout with timeout (sync wait answer (+/-) or break on timeout)
; TODO Вместо get-field и подобного хороши были бы макросы, например:
	;connection.port, connection.port = value
	;struct1 -> 'field >> struct2 (copy-field struct1 struct2 'field)
	;(@ 'p 'a 't 'h struct)
; TODO synchronous logout by contract

(module l2apf racket/base
	(require
		srfi/1
		racket/function
		(except-in racket/contract any)
		"api/connect.scm"
		"api/login.scm"
		"api/logout.scm"
		"api/refresh_manor_list.scm"
		"api/refresh_quest_list.scm"
		"api/refresh_skill_list.scm"
		"api/restart.scm"
		"api/select_character.scm"
		"api/select_server.scm"
		"api/social_action.scm"
		"library/structure.scm"
		"system/time_thread.scm"
	)
	(provide
		connect
		login
		select-server
		select-character
		refresh-manor-list
		refresh-quest-list
		refresh-skill-list
		social-action
		restart
		logout
	)
	(provide (contract-out
		(set-timeout (box? symbol? (or/c integer? false/c) . -> . void?))
		(set-interval (box? symbol? (or/c integer? false/c) . -> . void?))
		(get-events (box? . -> . evt?))
		(get-world (box? . -> . box?))
		(get-me (box? . -> . box?))
		;( ( . -> . ))
	))
	
	(define (get-me connection)
		(get-box-field connection 'me)
	)
	
	(define (get-world connection)
		(get-box-field connection 'world)
	)
	
	(define (get-events connection)
		(get-box-field connection 'events)
	)
	
	(define (set-timeout connection name timeout) ; precise timeout
		(let ((timeout (if timeout (+ (current-inexact-milliseconds) timeout) #f)))
			(let ((thread (get-box-field connection 'time-thread)))
				(thread-send thread (list 'set-timeout name timeout))
				(void)
			)
		)
	)
	
	(define (set-interval connection name time-or-fn) ; may have shifted
		(let ((fn (if (integer? time-or-fn) (const time-or-fn) time-or-fn)))
			(let ((thread (get-box-field connection 'time-thread)))
				(thread-send thread (list 'set-interval name fn))
				(void)
			)
		)
	)
	
	;(define (run-event name ?))
	;(define (set-event name checker handler))

)
