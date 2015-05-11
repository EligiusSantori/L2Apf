; Здесь все функции игровой логики, они не зависят от connection и оперируют чистыми данными
(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
		racket/dict
		"../library/geometry.scm"
	)
	(provide (contract-out
		(points-angle (point/3d? point/3d? . -> . (or/c real? false/c)))
		(points-distance (point/3d? point/3d? . -> . integer?))
		
		(struct-transfer (->* (dict? dict?) #:rest (listof symbol?) dict?))
	))

	(define (points-angle a b)
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