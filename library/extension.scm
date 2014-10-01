(module extension racket/base
	(require srfi/1)
	(provide
		bind
		bind-head
		bind-tail
		bind-wrap
		any-is
		every-is
	)
	
	(define (bind f . args)
		(lambda ()
			(apply f args)
		)
	)
	
	(define (bind-head f . head)
		(lambda args
			(apply f (append head args))
		)
	)
	
	(define (bind-tail f . tail)
		(lambda args
			(apply f (append args tail))
		)
	)
	
	(define (bind-wrap f head tail)
		(lambda args
			(apply f (append head args tail))
		)
	)
	
	; Хотя бы один элемент списка l равны значению v, используя для сравнения предикат p
	(define (any-is v l . t)
		(define p (if (null? t) equal? (car t)))
		(define (is i) (p v i))
		(apply any is l)
	)
	
	; Все элементы списка l равен значению v, используя для сравнения предикат p
	(define (every-is v l . t)
		(define p (if (null? t) equal? (car t)))
		(define (is i) (p v i))
		(apply every is l)
	)
)