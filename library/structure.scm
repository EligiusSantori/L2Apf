(module structure racket/base
	(require
		(rename-in racket/contract (any all/c))
		srfi/1
		racket/dict
		"extension.scm"
	)
	(provide (contract-out
		(@: (->* ((or/c dict? box?)) #:rest (listof any/c) any/c))
		(set-box-field! (box? symbol? any/c . -> . void?))
	))
	
	(define (set-field struct field value)
		(let ((struct (alist-delete field struct)))
			(if value (alist-cons field value struct) struct)
		)
	)
	
	(define (set-box-field! struct field value)
		(let ((new (set-field (unbox struct) field value)))
			(set-box! struct new)
			(void)
		)
	)

	; TODO struct-ref
	(define (@: struct . chain) ; ultimate dictionary path extractor
		(define (f a b) (if b (dict-ref (if (box? b) (unbox b) b) a #f) #f))
		(fold f (f (car chain) struct) (cdr chain))
	)
)
