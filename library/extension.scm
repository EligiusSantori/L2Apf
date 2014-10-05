(module extension racket/base
	(require srfi/1)
	(provide
		bind
		bind-head
		bind-tail
		bind-wrap
		any-is?
		every-is?
		alist-flip
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
	(define (any-is? v l . t)
		(define p (if (null? t) equal? (car t)))
		(define (is i) (p v i))
		(apply any is l)
	)
	
	; Все элементы списка l равен значению v, используя для сравнения предикат p
	(define (every-is? v l . t)
		(define p (if (null? t) equal? (car t)))
		(define (is i) (p v i))
		(apply every is l)
	)
	
	(define (alist-flip l)
		(map (compose xcons car+cdr) l)
	)
	
	;(define-syntax letone (syntax-rules () (
	;	(letone id value body ...)
	;	((lambda (id) body ...) value)
	;)))
)