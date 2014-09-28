; TODO Разбить на модули, особенно сеть вынести и упростить с ней работу, можно даже каждую функцию отдельной сделать
; TODO Сообщения для пакетов ошибок
	; TODO Новый концепт: (make-sync-operation handler event-checker); (make-contract handler event-checker)
; TODO Лучше два события со своими регулярными параметрами, чем одно с переключающимися!
; TODO Ключевые пакеты для ключевых событий
; TODO logout with timeout (sync wait answer (+/-) or break on timeout)
; TODO Усовершенствовать методы работы со структурами и вынести в библиотеку structure
; TODO Вместо get-field и подобного хороши были бы макросы (например "-> connection 'port port", "<- connection 'port")

(module l2apf racket/base
	(require
		srfi/1
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
		(all-defined-out)
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
	

	
	(define (get-event connection)
		(void) ; TODO Получаем пакет и обрабатываем. Если с ним связана генерация события, то возвращаем событие, иначе читаем дальше
	)
)
