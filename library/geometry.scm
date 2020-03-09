(module library racket/base
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
		point/3d->point/2d
		point/2d->point/3d
		point/3d=
		point/2d=
		point=

		pi/4 ; 45 deg
		pi/2 ; 90 deg
		3/2pi ; 270 deg
		2pi ; 360 deg

		distance/2d
		distance/3d
		angle/2d
		;angles/3d
		in-circle?
		in-sphere?
		in-sector?
		circle-point
		sphere-point
		circumference
		segment-offset/2d
		segment-offset/3d
		split-segment/2d
		split-segment/3d
	)

	(struct point/2d (x y) #:transparent)
	(struct point/3d (x y z) #:transparent)

	(define pi/4 (/ pi 4))
	(define pi/2 (/ pi 2))
	(define 3/2pi (* pi 3/2))
	(define 2pi (* 2 pi))

	(define (square n)
		(* n n)
	)

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

	(define (angle/2d a b) ; Angle between two points in radians
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

	(define (in-circle? a b r)
		(<= (distance/2d a b) r)
	)

	(define (in-sphere? a b r)
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

	(define (segment-offset/2d a b d)
		(let ((ratio (/ d (distance/2d a b))))
			(point/2d
				(+ (point/3d-x a) (* (- (point/2d-x a) (point/2d-x b)) ratio))
				(+ (point/3d-y a) (* (- (point/2d-y a) (point/2d-y b)) ratio))
			)
		)
	)

	(define (segment-offset/3d a b d)
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

	(define (point/2d= p1 p2)
		(and
			(= (point/2d-x p1) (point/2d-x p2))
			(= (point/2d-y p1) (point/2d-y p2))
		)
	)
	(define (point/3d= p1 p2)
		(and
			(= (point/3d-x p1) (point/3d-x p2))
			(= (point/3d-y p1) (point/3d-y p2))
			(= (point/3d-z p1) (point/3d-z p2))
		)
	)
	(define (point= p1 p2)
		(cond
			((and (point/2d? p1) (point/2d? p2)) (point/2d= p1 p2))
			((and (point/3d? p1) (point/3d? p2)) (point/3d= p1 p2))
			(else #f)
		)
	)

	(define (revert-angle a) ; Инвертирует направление шкалы сохраняя точку отсчёта
		(- 2pi a)
	)

	(define (simple-angle a) ; Удаляет лишние обороты и отрицательные углы
		(let ((b (mod a 2pi)))
			(if (< b 0) (+ 2pi b) b)
		)
	)

	(define (circumference r) ; Длинна окружности
		(* 2pi r)
	)

	(define (in-sector? p c a l)
		(let ((ra (angle/2d c p)))
			(and (>= a ra) (<= ra (+ a l)))
		)
	)
)
