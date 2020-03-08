(module ai racket/base
	(require
		srfi/1
		racket/undefined
		(relative-in "../.."
			"library/extension.scm"
		)
	)
	(provide
		program
		define-program
		program?
		program-id
		program-equal?
		program-load!
		program-free!
		program-run!
	)

	(struct base-program (
		id ; Unique progam id
		iterator ; Procedure that implements the program iteration.
		constructor ; Procedure that should be executed when the program loading.
		destructor ; Procedure that should be executed when the program unloading.
		config
		[state #:mutable]
		; compatible : list
		; incompatible : list
		; dependencies : list
	))

	(define program? base-program?)
	(define program-id base-program-id)

	(define-syntax-rule (define-program ID CONFIG CONSTRUCTOR DESTRUCTOR ITERATOR)
		(define ID (base-program
			'ID
			ITERATOR
			(if (eq? CONSTRUCTOR undefined) void CONSTRUCTOR)
			(if (eq? DESTRUCTOR undefined) void DESTRUCTOR)
			(if (eq? CONFIG undefined) (list) CONFIG)
			undefined
		))
	)

	(define (program base . config) ; Create configured instance of the program (instantiate).
		(define (list-merge to from)
			(define (equalize a b)
				(let ((al (length a)) (bl (length b)))
					(let ((tail (make-list (abs (- al bl)) undefined)))
						(cond
							((> al bl) (values a (append b tail)))
							((> bl al) (values (append a tail) b))
							(else (values a b))
						)
					)
				)
			)

			(apply map (lambda (t f)
				(if (eq? f undefined) t f)
			) (values->list (equalize to from)))
		)

		(let ((source (base-program-config base)))
			(struct-copy base-program base
				[config (if (and (not (null? config)))
					(if (alist? config)
						(alist-merge source config) ; TODO deep recursive merge
						(list-merge source config)
					)
					source ; Keep config as is.
				)]
			)
		)
	)

	(define (state? state)
		(not (or
			(void? state) ; Keep previous state.
			(eof-object? state) ; Terminate program.
		))
	)

	(define (program-load! cn program)
		(let ((state ((base-program-constructor program) cn (base-program-config program))))
			(when (state? state) (set-base-program-state! program state))
			(void)
		)
	)

	(define (program-free! cn program)
		((base-program-destructor program) cn (base-program-config program) (base-program-state program))
	)

	(define (program-run! cn p event) ; Iterate program for an event.
		(let ((state ((base-program-iterator p) cn event (base-program-config p) (base-program-state p))))
			(when (state? state) (set-base-program-state! p state))
			(not (eof-object? state))
		)
	)

	(define (program-equal? p1 p2)
		(and
			(program? p1) (program? p2)
			(eq? (program-id p1) (program-id p2))
		)
	)
)
