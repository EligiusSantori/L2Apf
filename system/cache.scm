(module system racket/base
	(require
		racket/contract
		racket/math
		racket/set
		data/splay-tree
	)
	(provide (contract-out
		(make-cache-set (->* (integer?) (procedure?) cache-set?))
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
		closest
		index
		data
	) #:mutable)

	(define (cache-set-find st it is?)
		(if it
			(let ((k (splay-tree-iterate-key st it)) (v (splay-tree-iterate-value st it)))
				(if (not (is? v))
					(cache-set-find st (splay-tree-iterate-next st it) is?)
					k
				)
			)
			#f
		)
	)
	(define (cache-set-expired st it until [vl (list)])
		(if it
			(let ((k (splay-tree-iterate-key st it)) (v (splay-tree-iterate-value st it)))
				(if (<= k until) ; Удаляем с expire = now, чтоб closest оказался подальше.
					(cache-set-expired st (splay-tree-iterate-next st it) until (cons v vl))
					(cons it vl)
				)
			)
			(cons it vl)
		)
	)
	(define (cache-set-clean! cs) ; Удаляем истёкшие, нежели копируем здоровые т.к. при частом обращении O(expired) < O(valid).
		(let ((ts (current-milliseconds)) (closest (cache-set-closest cs)) (index (cache-set-index cs)) (data (cache-set-data cs)))
			(when (and (> closest 0) (> ts closest))
				(let ((r (cache-set-expired index (splay-tree-iterate-least index) ts)))
					(set-subtract! data (apply seteq (cdr r)))
					(splay-tree-remove-range! index 0 ts)
					(set-cache-set-closest! cs (if (car r) (splay-tree-iterate-key index (car r)) 0))
				)
			)
		)
	)

	(define (make-cache-set lifetime [eqp eq?])
		(cache-set lifetime eqp 0 (make-adjustable-splay-tree) (mutable-seteq))
	)

	(define (cache-set-add! cs value [lifetime #f])
		(let ((closest (cache-set-closest cs)) (expire (exact-round (+ (current-milliseconds) (* (or lifetime (cache-set-lifetime cs)) 1000)))))
			(when (set-member? (cache-set-data cs) value)
				(cache-set-delete! cs value)
			)
			(when (or (zero? closest) (< expire closest))
				(set-cache-set-closest! cs expire)
			)
			(splay-tree-set! (cache-set-index cs) expire value)
			(set-add! (cache-set-data cs) value)
		)
	)

	(define (cache-set-delete! cs value)
		(when (and (not (cache-set-empty? cs)) (set-member? (cache-set-data cs) value))
			(let ((index (cache-set-index cs)) (eqp (cache-set-equal cs)) (ts (current-milliseconds)))
				(let ((key (cache-set-find index (splay-tree-iterate-least/>? index ts) (lambda (v) (eqp v value)))))
					(when key (splay-tree-remove! index key))
					(set-remove! (cache-set-data cs) value)
				)
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
		(zero? (cache-set-closest cs))
	)

	(define (cache-set-clear! cs)
		(set-cache-set-closest! cs 0)
		(set-cache-set-index! cs (make-adjustable-splay-tree))
		(set-cache-set-data! cs (mutable-seteq))
	)
)
