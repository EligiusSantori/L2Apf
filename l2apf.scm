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
		racket/contract
		"library/structure.scm"
	)
	(provide (contract-out
		(get-events (box? . -> . evt?))
		(get-world (box? . -> . box?))
		(get-me (box? . -> . box?))
	))
	
	(define (get-me connection)
		(@: connection 'me)
	)
	
	(define (get-world connection)
		(@: connection 'world)
	)
	
	(define (get-events connection)
		(@: connection 'event-channel)
	)
)
