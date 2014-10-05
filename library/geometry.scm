; Чтоб найти принадлежность к сектору круга, достаточно проверить попадает ли точка в круг + найти угол к ней, угол должен быть между двумя углами сектора
; Все формулы сюда, например окружность = 2piR

(module packet racket/base
	(require
		srfi/1
		racket/math
	)
	(provide
		(struct-out point/2d)
		(struct-out point/3d)
		distance/2d
		distance/3d
		angle/2d
		;angles/3d
		circle-point
		sphere-point
		line-segment-part/2d
		line-segment-part/3d
		split-line-segment/2d
		split-line-segment/3d
	)

	(struct point/2d (x y) #:transparent)
	(struct point/3d (x y z) #:transparent)
	
	(define (square n)
		(* n n)
	)
	
	(define (centrate/2d a b)
		(point/2d
			(- (point/2d-x b) (point/2d-x a))
			(- (point/2d-y b) (point/2d-y a))
		)
	)
	
	(define (centrate/3d a b)
		(point/3d
			(- (point/3d-x b) (point/3d-x a))
			(- (point/3d-y b) (point/3d-y a))
			(- (point/3d-z b) (point/3d-z a))
		)
	)
	
	; Нахождение точки на окружности
	(define (circle-point center radius angle)
		(point/2d
			(+ (point/2d-x center) (* radius (sin angle)))
			(+ (point/2d-y center) (* radius (cos angle)))
		)
	)
	
	; Нахождение точки на сфере
	(define (sphere-point center radius angle1 angle2)
		(point/3d
			(+ (point/3d-x center) (* radius (sin angle1) (cos angle2)))
			(+ (point/3d-y center) (* radius (sin angle1) (sin angle2)))
			(+ (point/3d-z center) (* radius (cos angle1)))
		)
	)
	
	(define (angle/2d a b) ; Наверняка можно оптимизировать
		(let* ((p (centrate/2d a b)) (x (point/2d-x p)) (y (point/2d-y p)))
			(cond
				((and (zero? x) (zero? y)) #f) ; points equal
				;((zero? x) (if (> y 0) 0 pi))
				;((zero? y) (if (> x 0) (* 1/2 pi) (* 3/2 pi)))
				((and (>= x 0) (> y 0)) (atan (abs x) (abs y)))
				((and (> x 0) (<= y 0)) (+ (atan (abs y) (abs x)) (* 1/2 pi)))
				((and (<= x 0) (< y 0)) (+ (atan (abs x) (abs y)) pi))
				((and (< x 0) (>= y 0)) (+ (atan (abs y) (abs x)) (* 3/2 pi)))
			)
		)
	)
	
	;(define (angles/3d a b)
	; TODO
	;)
	
	(define (in-sight?/2d a b r)
		(<= (distance/2d a b) r)
	)
	
	(define (in-sight?/3d a b r)
		(<= (distance/3d a b) r)
	)

	(define (in-square?/2d a b p)
		(let-values (((x y) (point/2d-x point/2d-y)))
			(and
				(<= (min (x a) (x b)) (x p) (max (x a) (x b)))
				(<= (min (y a) (y b)) (y p) (max (y a) (y b)))
			)
		)
	)

	(define (in-square?/3d a b p)
		(let-values (((x y z) (point/3d-x point/3d-y point/3d-z)))
			(and
				(<= (min (x a) (x b)) (x p) (max (x a) (x b)))
				(<= (min (y a) (y b)) (y p) (max (y a) (y b)))
				(<= (min (z a) (z b)) (z p) (max (z a) (z b)))
			)
		)
	)
	
	(define (distance/2d a b)
		(sqrt (+
			(square (- (point/2d-x a) (point/2d-x b)))
			(square (- (point/2d-y a) (point/2d-y b)))
		))
	)
	
	(define (distance/3d a b)
		(sqrt (+
			(square (- (point/3d-x a) (point/3d-x b)))
			(square (- (point/3d-y a) (point/3d-y b)))
			(square (- (point/3d-z a) (point/3d-z b)))
		))
	)
	
	(define (split-line-segment/2d a b n) ; TODO without ends: f(0, 10) = 5
		(define (f i)
			(point/2d
				(+ (point/2d-x a) (* (/ (- (point/2d-x b) (point/2d-x a)) (- n 1)) i))
				(+ (point/2d-y a) (* (/ (- (point/2d-y b) (point/2d-y a)) (- n 1)) i))
			)
		)
		(define (r l c)
			(if (>= c 0)
				(r (cons (f c) l) (- c 1))
				l
			)
		)
		(r (list) (- n 1))
	)
	
	(define (split-line-segment/3d a b n) ; TODO without ends: f(0, 10) = 5
		(define (f i)
			(point/3d
				(+ (point/3d-x a) (* (/ (- (point/3d-x b) (point/3d-x a)) (- n 1)) i))
				(+ (point/3d-y a) (* (/ (- (point/3d-y b) (point/3d-y a)) (- n 1)) i))
				(+ (point/3d-z a) (* (/ (- (point/3d-z b) (point/3d-z a)) (- n 1)) i))
			)
		)
		(define (r l c)
			(if (>= c 0)
				(r (cons (f c) l) (- c 1))
				l
			)
		)
		(r (list) (- n 1))
	)

	(define (line-segment-part/2d a b d)
		(let ((ratio (/ (distance/2d a b) d)))
			(point/2d
				(/ (- (point/2d-x a) (point/2d-x b)) ratio)
				(/ (- (point/2d-y a) (point/2d-y b)) ratio)
			)
		)
	)
	
	(define (line-segment-part/3d a b d)
		(let ((ratio (/ (distance/3d a b) d)))
			(point/3d
				(/ (- (point/3d-x a) (point/3d-x b)) ratio)
				(/ (- (point/3d-y a) (point/3d-y b)) ratio)
				(/ (- (point/3d-z a) (point/3d-z b)) ratio)
			)
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
