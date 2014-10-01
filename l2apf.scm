; TODO Сообщения для пакетов ошибок
; TODO Лучше два события со своими регулярными параметрами, чем одно с переключающимися!
; TODO Ключевые пакеты и ключевые события: social-action, use-item, use-skill, move-to, action, target; message, creature-update
; TODO logout with timeout (sync wait answer (+/-) or break on timeout)
; TODO Вместо get-field и подобного хороши были бы макросы, например:
	;connection.port, connection.port = value
	;struct1 -> 'field >> struct2 (copy-field struct1 struct2 'field)
; TODO synchronous logout by contract

(module l2apf racket/base
	(require
		srfi/1
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
		"library/structure.scm"
		"library/system.scm"
	)
	(provide
		connect
		login
		select-server
		select-character
		refresh-manor-list
		refresh-quest-list
		refresh-skill-list
		restart
		logout
	)
	(provide (contract-out
		(set-timeout (box? symbol? (or/c integer? false/c) . -> . void?))
		(set-interval (box? symbol? (or/c integer? false/c) . -> . void?))
		(get-events (box? . -> . box?))
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
	
	(define (set-timeout connection name time)
		(define main-thread (get-box-field connection 'thread))
		(thread-send main-thread (list 'set-timeout name time))
		(void)
	)
	
	(define (set-interval connection name time)
		(define main-thread (get-box-field connection 'thread))
		(thread-send main-thread (list 'set-interval name time))
		(void)
	)
	
	;(define (run-event name ?))
	;(define (set-event name checker handler))

)
