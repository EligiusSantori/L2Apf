(module structure racket/base
	(require
		(rename-in racket/contract (any all/c))
		srfi/1
		racket/dict
		"extension.scm"
	)
	(provide (contract-out
		(ref (->* ((or/c dict? box?)) #:rest (listof any/c) any/c))
		(@: (->* ((or/c dict? box?)) #:rest (listof any/c) any/c)) ; TODO deprecated
		(struct-transfer (->* (dict? dict?) #:rest (listof symbol?) dict?)) ; TODO deprecated
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

	(define (ref struct . chain) ; Ultimate dictionary path extractor.
		(define (f a b) (if (and b a) (dict-ref (if (box? b) (unbox b) b) a #f) #f))
		(fold f (f (car chain) struct) (cdr chain))
	)
	(define @: ref) ; TODO deprecated

	(define (struct-transfer target source . fields) ; TODO deprecated
		(alist-merge target (alist-only source fields eq?))
	)
)
