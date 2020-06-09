(module library racket/base
	(require
		(only-in racket/math exact-round)
		racket/contract
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

		(make-cache-hash (->* (integer?) (procedure?) cache-hash?))
		(cache-hash? (-> any/c boolean?))
		(cache-hash-set! (->* (cache-hash? any/c any/c) (integer?) void?))
		(cache-hash-ref (-> cache-hash? any/c any/c))
		(cache-hash-delete! (-> cache-hash? any/c void?))
		(cache-hash-empty? (-> cache-hash? boolean?))
		(cache-hash-count (-> cache-hash? integer?))
		(cache-hash-has? (-> cache-hash? any/c boolean?))
		(cache-hash-clear! (->* (cache-hash?) (boolean?) void?))
		(cache-hash->hash (-> cache-hash? hash?))
	))

	(define (expiration-<=? a b)
		(<= (car a) (car b))
	)
	(define (clean-index! wall heap deleted)
		(if (and (> (heap-count heap) 0))
			(let ((closest (heap-min heap)))
				(if (<= (car closest) wall) ; Удаляем с expire = now, чтоб следующий min оказался подальше.
					(begin
						(heap-remove-min! heap)
						(clean-index! wall heap (cons (cdr closest) deleted))
					)
					deleted
				)
			)
			deleted
		)
	)
	(define (index-delete! index value eqp)
		(heap-remove! index (cons 0 value) #:same? (lambda (a b) (eqp (cdr a) (cdr b))))
	)

	; Cache set.

	(struct cache-set (
		lifetime
		equal
		index
		data
	) #:mutable)

	(define (cache-set-clean! cs) ; Удаляем истёкшие, нежели копируем здоровые т.к. при частом обращении O(expired) < O(valid).
		(let ((deleted (clean-index! (current-milliseconds) (cache-set-index cs) (list))))
			(set-subtract! (cache-set-data cs) (apply seteq deleted))
		)
	)

	(define (make-cache-set lifetime [eqp eq?])
		(cache-set lifetime eqp (make-heap expiration-<=?) (mutable-seteq))
	)

	(define (cache-set-add! cs value [lifetime #f])
		(let ((expire (exact-round (+ (current-milliseconds) (* (or lifetime (cache-set-lifetime cs)) 1000)))))
			(let ((data (cache-set-data cs)) (index (cache-set-index cs)))
				(if (set-member? data value)
					(index-delete! index value (cache-set-equal cs))
					(set-add! data value)
				)
				(heap-add! index (cons expire value))
			)
		)
	)

	(define (cache-set-delete! cs value)
		(when (and (not (cache-set-empty? cs)) (set-member? (cache-set-data cs) value))
			(index-delete! (cache-set-index cs) value (cache-set-equal cs))
			(set-remove! (cache-set-data cs) value)
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

	; Cache hash.

	(struct cache-hash (
		lifetime
		callback
		index
		data
	) #:mutable)

	(define (cache-hash-clean! ch) ; Удаляем истёкшие, нежели копируем здоровые т.к. при частом обращении O(expired) < O(valid).
		(let ((data (cache-hash-data ch)) (callback (cache-hash-callback ch)))
			(map (lambda (key)
				(let ((value (hash-ref data key)))
					(hash-remove! data key)
					(callback key value)
				)
			) (clean-index! (current-milliseconds) (cache-hash-index ch) (list)))
		)
	)

	(define (make-cache-hash lifetime [callback void])
		(cache-hash lifetime callback (make-heap expiration-<=?) (make-hasheq))
	)

	(define (cache-hash-set! ch key value [lifetime #f])
		(let ((expire (exact-round (+ (current-milliseconds) (* (or lifetime (cache-hash-lifetime ch)) 1000)))))
			(when (hash-has-key? (cache-hash-data ch) key) (index-delete! (cache-hash-index ch) key eq?))
			(heap-add! (cache-hash-index ch) (cons expire key))
			(hash-set! (cache-hash-data ch) key value)
		)
	)

	(define (cache-hash-ref ch key [default #f])
		(cache-hash-clean! ch)
		(hash-ref (cache-hash-data ch) key default)
	)

	(define (cache-hash-delete! ch key)
		(let ((data (cache-hash-data ch)))
			(when (hash-has-key? data key)
				(let ((value (hash-ref data key)))
					(index-delete! (cache-hash-index ch) key eq?)
					(hash-remove! data key)
					((cache-hash-callback ch) key value)
				)
			)
		)
		(void)
	)

	(define (cache-hash-empty? ch)
		(cache-hash-clean! ch)
		(hash-empty? (cache-hash-data ch))
	)

	(define (cache-hash-count ch)
		(cache-hash-clean! ch)
		(hash-count (cache-hash-data ch))
	)

	(define (cache-hash-has? ch key)
		(cache-hash-clean! ch)
		(hash-has-key? (cache-hash-data ch) key)
	)

	(define (cache-hash-clear! ch [no-callback #t])
		(when (not no-callback) (hash-map (cache-hash-data ch) (cache-hash-callback ch)))
		(set-cache-hash-index! ch (make-heap expiration-<=?))
		(set-cache-hash-data! ch (make-hasheq))
	)

	(define (cache-hash->hash ch)
		(cache-hash-clean! ch)
		(hash-copy (cache-hash-data ch))
	)
)
