(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
		"../library/geometry.scm"
	)
	(provide (contract-out
		(points-angle (point/3d? point/3d? . -> . (or/c rational? false/c)))
		(points-distance (point/3d? point/3d? . -> . integer?))
	))

	(define (points-angle a b)
		(angle/2d
			(point/2d (point/3d-x a) (- (point/3d-y a)))
			(point/2d (point/3d-x b) (- (point/3d-y b)))
		)
	)

	(define (points-distance a b)
		(round (distance/3d a b))
	)
)
