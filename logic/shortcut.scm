(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
		srfi/1
		srfi/42
	)

	(provide (contract-out
		;(listen-event! (->* (box? symbol? (or/c procedure? false/c)) #:rest (or/c false/c (listof any/c)) void?))
		(shortcut? (any/c . -> . boolean?))
		(shortcut-ref (vector? integer? integer? . -> . (or/c shortcut? false/c)))
		(make-shortcut (->* (symbol? integer?) #:rest (or/c false/c (listof any/c)) shortcut?))
		;(make-shortcut-bar)
		;(make-shortcut-desk)
	))
	
	;shortcut-type/item
	;shortcut-type/skill
	;shortcut-type/action
	
	(define (shortcut? s)
		(and
			(> (length s) 1)
			(symbol? (first s))
			(integer? (second s))
		)
	)
	
	(define (shortcut-ref desk bar slot)
		(vector-ref (vector-ref desk bar) slot)
	)
	
	(define (make-shortcut type id . tail)
		(if (null? tail)
			(list type id)
			(list type id (car tail))
		)
	)
	
	(define (make-shortcut-bar)
		(make-vector 12 #f)
	)
	
	(define (make-shortcut-desk)
		(vector-ec (: i 10) (make-shortcut-bar))
	)
)