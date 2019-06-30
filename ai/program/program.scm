(module ai racket/base
	(require
		srfi/1
		racket/undefined
		(relative-in "../../.."
			"library/extension.scm"
		)
	)
	(provide
		; ai-program
		; ai-program?
		; ai-program-id
		(struct-out ai-program) ; Required for struct-copy.
		(rename-out (equal? ai-program-equal?))
		(rename-out (make ai-program-make))
		(rename-out (load! ai-program-load!))
		(rename-out (free! ai-program-free!))
		(rename-out (run! ai-program-run!))
		ai-program-base
	)

	(struct ai-program (
			id ; Unique progam id
			iterator ; Procedure that implements the program iteration.
			constructor ; Procedure that should be executed when the program loading.
			destructor ; Procedure that should be executed when the program unloading.
			config
			[state #:mutable]
			; compatible : list
			; incompatible : list
			; dependencies : list
		)
	)

	(define (make base . config) ; Create configured instance of the program.
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

		(let ((source (ai-program-config base)))
			(struct-copy ai-program base
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
		(and (not (void? state)) (not (eq? undefined state)))
	)

	(define (load! program)
		(let ((state ((ai-program-constructor program) (ai-program-config program))))
			(when (state? state) (set-ai-program-state! program state))
			state
		)
	)

	(define (free! program)
		((ai-program-destructor program) (ai-program-config program) (ai-program-state program))
	)

	(define (run! program event connection) ; Iterate program for an event.
		(let ((state ((ai-program-iterator program) event connection (ai-program-config program) (ai-program-state program))))
			(when (state? state) (set-ai-program-state! program state))
			state
		)
	)

	(define (equal? p1 p2)
		(and
			(ai-program? p1) (ai-program? p2)
			(eq? (ai-program-id p1) (ai-program-id p2))
		)
	)

	(define ai-program-base (ai-program
		undefined ; id (required)
		undefined ; iterator (required)
		(lambda args (void)) ; default constructor (do nothing)
		(lambda args (void)) ; default destructor (do nothing)
		(list) ; empty config
		undefined ; empty state
	))
)
