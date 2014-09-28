(module structure racket/base
	(require (except-in racket/contract any) srfi/1)
	(provide (contract-out
		(get-field (list? symbol? . -> . any/c))
		(set-field (list? symbol? any/c . -> . list?))
		(get-fields (->* (list?) #:rest (listof symbol?) (listof any/c)))
		(get-chain (->* (list?) #:rest (listof symbol?) any/c))
		(get-box-field (box? symbol? . -> . any/c))
		(set-box-field! (box? symbol? any/c . -> . void?))
	))
	
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
		(void) ; TODO
	)
	
	(define (get-chain struct . chain)
		(void) ; TODO
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