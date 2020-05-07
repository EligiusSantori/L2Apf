(module ai racket/base
	(require
		(only-in srfi/1 make-list)
		(for-syntax racket/base)
		(for-syntax syntax/parse)
		racket/string
		racket/contract
		racket/undefined
		(relative-in "../.."
			"library/extension.scm"
			"system/error.scm"
			"system/log.scm"
			"system/connection.scm"
			"system/event.scm"
		)
	)
	(provide
		define-program
		(struct-out exn:error:program)
		(contract-out
			(program (->* (program?) #:rest any/c program?))
			(program? (-> any/c boolean?))
			(program-id (-> program? symbol?))
			(program-equal? (-> program? program? boolean?))
			(program-load! (-> connection? program? boolean?))
			(program-free! (-> connection? program? void?))
			(program-run! (-> connection? program? event? boolean?))
			(raise-program-error (->* (symbol? string?) #:rest any/c any))
		)
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

	(define-syntax (define-program STX)
		(syntax-parse STX
			((define-program ID ITERATOR
					(~optional (~seq #:constructor CONSTRUCTOR))
					(~optional (~seq #:destructor DESTRUCTOR))
					(~optional (~seq #:defaults CONFIG)))
				(with-syntax (
						(CONSTRUCTOR (or (attribute CONSTRUCTOR) void))
						(DESTRUCTOR (or (attribute DESTRUCTOR) void))
						(CONFIG (or (attribute CONFIG) #'(list))))
					#'(define ID (base-program (quote ID) ITERATOR CONSTRUCTOR DESTRUCTOR CONFIG undefined))
				)
			)
		)
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
					(if (alist? source)
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

	(define (program-load! cn p)
		(apf-debug "Loading program ~a." (program-id p))
		(let ((state ((base-program-constructor p) cn (base-program-config p))))
			(when (state? state) (set-base-program-state! p state))
			(not (eof-object? state))
		)
	)

	(define (program-free! cn p)
		(apf-debug "Unloading program ~a." (program-id p))
		((base-program-destructor p) cn (base-program-config p) (base-program-state p))
		(void)
	)

	(define (program-run! cn p event) ; Iterate program for an event.
		(let ((state ((base-program-iterator p) cn event (base-program-config p) (base-program-state p))))
			(when (state? state) (set-base-program-state! p state))
			(not (eof-object? state))
		)
	)

	(define (program-equal? p1 p2)
		(eq? (program-id p1) (program-id p2))
	)

	(struct exn:error:program exn:fail:user (id)
		#:extra-constructor-name make-exn:error:program
		#:transparent
	)
	(define (raise-program-error program-id message . args)
		(raise (make-exn:error:program (error-format message args) (current-continuation-marks) program-id))
	)
)
