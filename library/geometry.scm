(module packet racket/base
	(provide point2d)
	(provide point3d)

	(struct point2d (x y) #:transparent)
	(struct point3d (x y z) #:transparent)
	
	; Нахождение точки на окружности
	(define (get-course2d center radius angle)
		(point2d
			(+ (point2d-x center) (* radius (sin angle)))
			(+ (point2d-y center) (* radius (cos angle)))
		)
	)
	
	; Нахождение точки на сфере
	(define (get-course3d center radius angle1 angle2)
		(point3d
			(+ (point3d-x center) (* radius (sin angle1) (cos angle2)))
			(+ (point3d-y center) (* radius (sin angle1) (sin angle2)))
			(+ (point3d-z center) (* radius (cos angle1)))
		)
	)
	
	; degrees->radians
	;(define (deg-to-rad deg) 
	;	(* deg (/ pi 180))
	;)
	
	; radians->degrees
	;(define (rad-to-deg rad) 
	;	(* rad (/ 180 pi))
	;)
)