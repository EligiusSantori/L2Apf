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
		program-lambda
		(struct-out exn:error:program)
		(contract-out
			(make-program (->* (symbol? procedure?) (#:constructor procedure? #:destructor procedure?) program?))
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
		; config
		[state #:mutable]
		; compatible : list
		; incompatible : list
		; dependencies : list
	))

	(define program? base-program?)
	(define program-id base-program-id)

	(define (make-program id iterator #:constructor [constructor void] #:destructor [destructor void])
		(base-program id iterator constructor destructor undefined)
	)
	(define-syntax program-lambda
		(syntax-rules ()
			((_ (EVENT (STATE INIT)) . BODY)
				(base-program (gensym) (lambda (connection EVENT STATE) . BODY) void void INIT)
			)
			((_ (EVENT STATE) . BODY)
				(program-lambda (EVENT [STATE undefined]) . BODY)
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
		(let ((state ((base-program-constructor p) cn)))
			(when (state? state) (set-base-program-state! p state))
			(not (eof-object? state))
		)
	)

	(define (program-free! cn p)
		(apf-debug "Unloading program ~a." (program-id p))
		((base-program-destructor p) cn (base-program-state p))
		(void)
	)

	(define (program-run! cn p event) ; Iterate program for an event.
		(let ((state ((base-program-iterator p) cn event (base-program-state p))))
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
