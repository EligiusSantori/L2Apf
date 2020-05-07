(module library racket/base
	(require
		racket/contract
		(only-in racket/math exact-round)
		racket/set
		data/heap
	)
	(provide (contract-out
		(make-cache-set (->* (integer?) (procedure?) cache-set?))
		(cache-set? (-> any/c boolean?))
		(cache-set-add! (->* (cache-set? any/c) (integer?) void?))
		(cache-set-delete! (-> cache-set? any/c void?))
		(cache-set-clear! (-> cache-set? void?))
		(cache-set-all (-> cache-set? generic-set?))
		(cache-set-has? (-> cache-set? any/c boolean?))
		(cache-set-empty? (-> cache-set? boolean?))
		(cache-set-count (-> cache-set? integer?))
	))

	(struct cache-set (
		lifetime
		equal
		index
		data
	) #:mutable)

	(define (expiration-<=? a b)
		(<= (car a) (car b))
	)

	(define (clean-index wall heap deleted)
		(if (and (> (heap-count heap) 0))
			(let ((closest (heap-min heap)))
				(if (<= (car closest) wall) ; Удаляем с expire = now, чтоб следующий min оказался подальше.
					(begin
						(heap-remove-min! heap)
						(clean-index wall heap (cons (cdr closest) deleted))
					)
					deleted
				)
			)
			deleted
		)
	)
	(define (cache-set-clean! cs) ; Удаляем истёкшие, нежели копируем здоровые т.к. при частом обращении O(expired) < O(valid).
		(let ((deleted (clean-index (current-milliseconds) (cache-set-index cs) (list))))
			(set-subtract! (cache-set-data cs) (apply seteq deleted))
		)
	)

	(define (make-cache-set lifetime [eqp eq?])
		(cache-set lifetime eqp (make-heap expiration-<=?) (mutable-seteq))
	)

	(define (cache-set-add! cs value [lifetime #f])
		(let ((expire (exact-round (+ (current-milliseconds) (* (or lifetime (cache-set-lifetime cs)) 1000)))))
			(when (set-member? (cache-set-data cs) value)
				(cache-set-delete! cs value)
			)
			(heap-add! (cache-set-index cs) (cons expire value))
			(set-add! (cache-set-data cs) value)
		)
	)

	(define (cache-set-delete! cs value)
		(when (and (not (cache-set-empty? cs)) (set-member? (cache-set-data cs) value))
			(let ((index (cache-set-index cs)) (eqp (cache-set-equal cs)))
				(heap-remove! index (cons 0 value) #:same? (lambda (a b) (eqp (cdr a) (cdr b))))
				(set-remove! (cache-set-data cs) value)
			)
		)
	)

	(define (cache-set-all cs)
		(cache-set-clean! cs)
		(cache-set-data cs)
	)

	(define (cache-set-count cs)
		(set-count (cache-set-all cs))
	)

	(define (cache-set-has? cs value)
		(set-member? (cache-set-all cs) value)
	)

	(define (cache-set-empty? cs)
		(set-empty? (cache-set-data cs))
	)

	(define (cache-set-clear! cs)
		(set-cache-set-index! cs (make-heap expiration-<=?))
		(set-cache-set-data! cs (mutable-seteq))
	)
)
