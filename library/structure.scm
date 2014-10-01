(module structure racket/base
	(require
		(rename-in racket/contract (any all/c))
		srfi/1
		"extension.scm"
	)
	(provide (contract-out
		;(get-field (list? symbol? . -> . any/c))
		(set-field (list? symbol? any/c . -> . list?))
		(get-fields (->* (list?) #:rest (listof symbol?) all/c))
		;(get-chain (->* (list?) #:rest (listof symbol?) any/c))
		(get-box-field (box? symbol? . -> . any/c))
		(set-box-field! (box? symbol? any/c . -> . void?))
	))
	(provide
		(rename-out (get-chain get-field))
	)
	
	(define (get-field struct field)
		(let ((value (assoc field struct)))
			(if value (cdr value) #f)
		)
	)
	(define (set-field struct field value)
		(let ((struct (alist-delete field struct)))
			(if value (alist-cons field value struct) struct)
		)
	)
	
	(define (get-fields struct . fields)
		(apply values (map (bind-head get-field struct) fields))
	)
	
	(define (get-chain struct . chain)
		(define (f a b) (if b (get-field b a) #f))
		(fold f (get-field struct (car chain)) (cdr chain))
	)
	
	(define (get-box-field struct field)
		(get-field (unbox struct) field)
	)
	(define (set-box-field! struct field value)
		(let ((new (set-field (unbox struct) field value)))
			(set-box! struct new)
			(void)
		)
	)
)