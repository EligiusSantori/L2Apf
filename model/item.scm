(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
		srfi/1
		"object.scm"
	)

	(provide (contract-out
		(item-on-ground? (object? . -> . boolean?))
		(inventory-item? (object? . -> . boolean?))
	))
	
	(define (item-on-ground? object)
		(void) ; TODO
	)
	
	(define (inventory-item? object)
		(void) ; TODO
	)
)