; Чтоб найти принадлежность к сектору круга, достаточно проверить попадает ли точка в круг + найти угол к ней, угол должен быть между двумя углами сектора
; Все формулы сюда, например окружность = 2piR

(module packet racket/base
	(require
		srfi/1
		(only-in rnrs/base-6 mod)
		racket/math
		(rename-in racket/contract (any all/c))
	)
	(provide (contract-out
		(revert-angle (rational? . -> . rational?))
		(simple-angle (rational? . -> . rational?))
	))
	(provide
		(struct-out point/2d)
		(struct-out point/3d)
		distance/2d
		distance/3d
		angle/2d
		;angles/3d
		circle-point
		sphere-point
		segment-offset/2d
		segment-offset/3d
		split-segment/2d
		split-segment/3d
		point/3d->point/2d
		point/2d->point/3d
	)

	(struct point/2d (x y) #:transparent)
	(struct point/3d (x y z) #:transparent)
	
	(define (square n)
		(* n n)
	)
	
	(define 2pi (* 2 pi))
	
	(define (centrate/2d a b) ; TODO segment-center/2d
		(point/2d
			(- (point/2d-x b) (point/2d-x a))
			(- (point/2d-y b) (point/2d-y a))
		)
	)
	
	(define (centrate/3d a b) ; TODO segment-center/3d
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
				(else (atan y x))
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
	
	;(define (in-circle? c r p)
		; TODO
	;)
	
	;(define (in-sphere? c r p)
		; TODO
	;)
	
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
	
	(define (split-segment/2d a b n) ; TODO without ends: f(0, 10) = 5
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
	
	(define (split-segment/3d a b n) ; TODO without ends: f(0, 10) = 5
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

	(define (segment-offset/2d a b d) ; TODO segment-offset/2d ; TODO round?
		(let ((ratio (/ d (distance/2d a b))))
			(point/2d
				(+ (point/3d-x a) (* (- (point/2d-x a) (point/2d-x b)) ratio))
				(+ (point/3d-y a) (* (- (point/2d-y a) (point/2d-y b)) ratio))
			)
		)
	)
	
	(define (segment-offset/3d a b d) ; TODO round?
		(let ((ratio (/ d (distance/3d a b))))
			(point/3d
				(+ (point/3d-x a) (* (- (point/3d-x b) (point/3d-x a)) ratio))
				(+ (point/3d-y a) (* (- (point/3d-y b) (point/3d-y a)) ratio))
				(+ (point/3d-z a) (* (- (point/3d-z b) (point/3d-z a)) ratio))
			)
		)
	)
	
	(define (point/3d->point/2d p)
		(point/2d (point/3d-x p) (point/3d-y p))
	)
	
	(define (point/2d->point/3d p [z 0])
		(point/3d (point/2d-x p) (point/2d-y p) z)
	)
	
	; Инвертирует направление шкалы сохраняя точку отсчёта
	(define (revert-angle a)
		(- 2pi a)
	)
	
	; Удаляет лишние обороты и отрицательные углы
	(define (simple-angle a) 
		(mod (+ 2pi a) 2pi)
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
