; Здесь все функции игровой логики, они не зависят от connection и оперируют чистыми данными
(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
		racket/dict
		"library/geometry.scm"
	)
	(provide (contract-out
		(points-angle (point/3d? point/3d? . -> . (or/c rational? false/c)))
		(points-distance (point/3d? point/3d? . -> . integer?))
		(struct-transfer (->* (dict? dict?) #:rest (listof symbol?) dict?))
	))

	; TODO Нужен какой-то конвертор странного угла lineage туда и обратно
		; Или угол и так в нормальном виде хранится, просто если вычислять от координат, то надо инвертировать?
		; Если нет, то может в пакетах на лету просто конвертировать?
		; Вомзонжо инвертированный угол будет правильным, если смотреть на игрока снизу
		; Надо посмотреть в каком виде сервер присылает угол, возможно это просто тот самый инвертированный и всё
	(define (points-angle a b) ; То же самое, что revert-angle от простого угла
		(angle/2d
			(point/2d (point/3d-x a) (- (point/3d-y a))) ; y axis is inverted
			(point/2d (point/3d-x b) (- (point/3d-y b))) ; y axis is inverted
		)
	)

	(define (points-distance a b)
		(round (distance/3d a b))
	)
	
	(define (struct-transfer target source . fields)
		(let ((fields (filter (lambda (i) (assoc i source)) fields))) ; only exist fields
			(append
				(filter (lambda (i) (member (car i) fields)) source)
				(filter (lambda (i) (not (member (car i) fields))) target)
			)
		)
	)
	
	; TODO pattern-filter & deep-merge instead of struct-transfer
)