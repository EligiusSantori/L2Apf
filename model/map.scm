(module logic racket/base
	(require
		racket/math
		(rename-in racket/contract (any all/c))
		"../library/geometry.scm"
	)
	(provide (contract-out
		(points-angle (-> point/3d? point/3d? (or/c rational? false/c)))
		(points-distance (-> point/3d? point/3d? integer?))
		(map-circle-point (-> point/3d? integer? rational? point/3d?))
	))

	(define (points-angle a b)
		(angle/2d
			(point/2d (point/3d-x a) (- (point/3d-y a)))
			(point/2d (point/3d-x b) (- (point/3d-y b)))
		)
	)

	(define (points-distance a b)
		(exact-round (distance/3d a b))
	)

	(define (map-circle-point center radius angle)
		(point/3d
			(exact-round (+ (point/3d-x center) (* radius (cos angle))))
			(exact-round (- (point/3d-y center) (* radius (sin angle))))
			(point/3d-z center)
		)
	)
)
