(module library racket/base ; Language extensions and implementaton abstraction layer.
	(require
		srfi/1
		racket/function
		(only-in racket/list flatten)
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
		alist-equal?
		string-starts?
		string-ends?
		list-try-ref
		try-first
		try-second
		try-third
		call/wv
		values->list
		list->values
		byte->hex
		char->hex
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
	(define (alist-equal? a b)
		(equal? (apply hasheq (flatten a)) (apply hasheq (flatten b)))
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

	(define (list-try-ref lst n [default #f])
		(cond
			((null? lst) default)
			((= n 0) (car lst))
			(else (list-try-ref (cdr lst) (- n 1) default))
		)
	)
	(define (try-first lst [default #f])
		(if (pair? lst) (car lst) default)
	)
	(define (try-second lst [default #f])
		(list-try-ref lst 1 default)
	)
	(define (try-third lst [default #f])
		(list-try-ref lst 2 default)
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

	(define (char->hex c)
		(case c
			(( 0) #\0) (( 1) #\1) (( 2) #\2) (( 3) #\3)
			(( 4) #\4) (( 5) #\5) (( 6) #\6) (( 7) #\7)
			(( 8) #\8) (( 9) #\9) ((10) #\a) ((11) #\b)
			((12) #\c) ((13) #\d) ((14) #\e) ((15) #\f)
		)
	)
	(define (byte->hex b)
		(let-values (((q r) (quotient/remainder b 16)))
			(string (char->hex q) (char->hex r))
		)
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
