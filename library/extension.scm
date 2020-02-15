(module library racket/base ; Language extensions and implementaton abstraction layer.
	(require
		srfi/1
		racket/function
		(for-syntax racket/base)
		(rename-in racket/contract (any all/c))
	)
	(provide
		bind
		bind-head
		bind-tail
		bind-wrap
		always?
		never?
		any-is?
		every-is?
		bubble
		; min-in
		; max-in
		alist?
		alist-flip
		alist-ref
		alist-only
		alist-merge
		string-starts?
		string-ends?
		length>
		try-first
		try-second
		try-third
		call/wv
		values->list
		list->values
		(contract-out
			(bytes->string/utf-16 (bytes? . -> . string?))
			(bytes->string/utf-16le (bytes? . -> . string?))
			(bytes->string/utf-16ge (bytes? . -> . string?))
			(string->bytes/utf-16 (string? . -> . bytes?))
			(string->bytes/utf-16le (string? . -> . bytes?))
			(string->bytes/utf-16ge (string? . -> . bytes?))
			(hash-filter (hash? procedure? . -> . list?))
			(hash-find (hash? procedure? . -> . any/c))
			(update-box! (box? procedure? . -> . void?))
			;(values->list (-> all/c list?))
		)
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

	(define always? (const #t))

	(define never? (const #f))

	; Хотя бы один элемент списка l равен значению v, используя для сравнения предикат p
	(define any-is? member) ; TODO remove

	; Все элементы списка l равны значению v, используя для сравнения предикат p
	(define (every-is? v l [p equal?])
		(define (is i) (p v i))
		(apply every is l)
	)

	(define (bubble c l [d #f])
		(reduce (lambda (e p)
			(if (c e p) e p)
		) d l)
	)

	(define (alist? l)
		(and
			(list? l)
			(not (null? l))
			(pair? (car l))
		)
	)

	(define (alist-flip lst)
		(map (compose xcons car+cdr) lst)
	)
	(define (alist-ref lst key [default #f] [eqp equal?])
		(let ((r (memf (lambda (p) (and (pair? p) (eqp key (car p)))) lst)))
			(if r (cdr (car r)) default)
		)
	)
	(define (alist-only lst keys [eqp equal?])
		(filter (lambda (p)
			(and
				(pair? p) ; Also filter out invalid pairs
				(member (car p) keys eqp)
			)
		) lst)
	)
	(define (alist-merge to from)
		(fold (lambda (p r) ; Just add elements missing in <from> from <to>
			(if (not (assoc (car p) r))
				(cons p r)
				r
			)
		) from to)
	)

	(define (string-starts? s t)
		(let ((ls (string-length s)) (lt (string-length t)))
			(and
				(>= ls lt)
				(string=? (substring s 0 lt) t)
			)
		)
	)
	(define (string-ends? s t)
		(let ((ls (string-length s)) (lt (string-length t)))
			(and
				(>= ls lt)
				(string=? (substring s (- ls lt)) t)
			)
		)
	)

	(define (length> lst n)
		(cond
			((null? lst) #f)
			((= n 0) #t)
			(else (length> (cdr lst) (- n 1)))
		)
	)
	(define (try-first lst [default #f])
		(if (length> lst 0) (car lst) default)
	)
	(define (try-second lst [default #f])
		(if (length> lst 1) (second lst) default)
	)
	(define (try-third lst [default #f])
		(if (length> lst 2) (third lst) default)
	)

	(define call/wv call-with-values)
	(define-syntax (values->list stx)
		(syntax-case stx ()
			((_ expr) #'(call-with-values (lambda () expr) list))
		)
	)
	(define (list->values lst)
		(apply values lst)
	)

	(define (convert data from to)
		(let ((e (bytes-open-converter from to)))
			(let-values (((u n r) (bytes-convert e data)))
				(begin
					(bytes-close-converter e)
					(if (equal? r 'complete) u #f)
				)
			)
		)
	)
	(define (bytes->string/utf-16 data)
		(let ((data (convert data "UTF-16" "")))
			(if data (bytes->string/locale data) #f)
		)
	)
	(define (string->bytes/utf-16 s)
		(let ((data (string->bytes/locale s)))
			(convert data "" "UTF-16")
		)
	)
	(define (bytes->string/utf-16le data)
		(let ((data (convert data "UTF-16LE" "")))
			(if data (bytes->string/locale data) #f)
		)
	)
	(define (string->bytes/utf-16le s)
		(let ((data (string->bytes/locale s)))
			(convert data "" "UTF-16LE")
		)
	)
	(define (bytes->string/utf-16ge data)
		(let ((data (convert data "UTF-16GE" "")))
			(if data (bytes->string/locale data) #f)
		)
	)
	(define (string->bytes/utf-16ge s)
		(let ((data (string->bytes/locale s)))
			(convert data "" "UTF-16GE")
		)
	)

	(define (hash-filter h p)
		(define l (list))
		(hash-for-each h (lambda (k v)
			(when (p k v)
				(set! l (cons (cons k v) l))
			)
		))
		l
	)
	(define (hash-find h p)
		(define i #f)
		(hash-for-each h (lambda (k v)
			(when (p k v)
				(set! i (cons k v)) ; TODO return immediately using continuation
			)
		))
		i
	)

	(define (update-box! b f)
		(set-box! b (f (unbox b)))
	)

	; (define-syntax-rule (let-list (((name ...) value) ...) body ...)
	; 	(let-values (((name ...) (list->values value)) ...)
	; 		(begin body ...)))

	; (define-syntax letone (syntax-rules () (
	; 	(letone id value body ...)
	; 	((lambda (id) body ...) value)
	; )))

	; syntax let-alist (((a b c) (k1 k2 k3) alist)) body
	; syntax let-list (((a b c) list)) body ; receive ?
)
