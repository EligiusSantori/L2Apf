(module ai racket/base
	(require
		srfi/1
		(only-in "program/program.scm"
			ai-program-equal?
			ai-program-load!
			ai-program-free!
			ai-program-run!
		)
		(relative-in "../.."
			"library/extension.scm"
		)
	)
	(provide
		make-ai-manager ; Make a program manager.
		(rename-out (run! ai-manager-run!))
		(rename-out (do! ai-manager-do!))
		(rename-out (load! ai-manager-load!))
		(rename-out (free! ai-manager-free!))
		(rename-out (clear! ai-manager-clear!))
	)

	(struct ai-manager (
			default ; Defaunt program.
			[main #:auto #:mutable] ; Foreground programs stack.
			[todo #:auto #:mutable] ; Background programs set, part 1.
			[done #:auto #:mutable] ; Background programs set, part 2.
			; [triggers #:auto #:mutable] ; Triggers set.
		)
		#:constructor-name make-ai-manager
		#:auto-value (list)
	)

	(define finished? eof-object?)

	(define (active manager) ; Get active foreground program (or default).
		(try-first (ai-manager-main manager) (ai-manager-default manager))
	)
	(define (count manager) ; Total programs count.
		(+
			(length (ai-manager-main manager))
			(length (ai-manager-done manager))
			(length (ai-manager-todo manager))
		)
	)

	(define (foreground? manager program) ; If program exists in foreground stack.
		(member program (ai-manager-main manager) ai-program-equal?)
	)
	(define (background? manager program) ; If program exists in background set.
		(or
			(member program (ai-manager-done manager) ai-program-equal?)
			(member program (ai-manager-todo manager) ai-program-equal?)
		)
	)
	(define (exists? manager program) ; If program exists in manager.
		(or
			(foreground? manager program)
			(background? manager program)
		)
	)

	(define (load! manager . programs) ; Load background programs.
		(set-ai-manager-todo! manager
			(append (ai-manager-todo manager) (filter (lambda (p)
				(and (not (exists? manager p)) ; Don't replace existsing (use free before).
					(begin (ai-program-load! p) p) ; Execute loading program constructor.
				)
			) programs))
		)
	)

	(define (filter-foreground! manager keep?)
		(let ((main (ai-manager-main manager))) (when (not (null? main))
			(let ((s (filter keep? (cdr main))))
				(set-ai-manager-main! manager
					(if (not (keep? (car main))) ; If active program should be unloaded.
						(begin
							(ai-program-free! (car main)) ; Execute current program destructor.
							(ai-program-load! (try-first s (ai-manager-default manager))) ; Execute unstacking program constructor.
							s
						)
						(cons (car main) s) ; Else just prepend to filtered stack.
					)
				)
			)
		))
	)

	(define (filter-background! manager keep?)
		(define (keep?+ p) (or (keep? p) (begin (ai-program-free! p) #f)))
		(set-ai-manager-todo! manager (filter keep?+ (ai-manager-todo manager)))
		(set-ai-manager-done! manager (filter keep?+ (ai-manager-done manager)))
	)

	(define (free! manager . programs) ; Unload amount of programs.
		(define (keep? p) (not (member p programs ai-program-equal?)))
		(when (not (null? programs))
			(filter-foreground! manager keep?)
			(filter-background! manager keep?)
		)
	)

	(define (clear! manager [foreground? #t] [background? #f] [except (list)]) ; Unload all foreground or/and background programs.
		(define (keep? p) (member p except ai-program-equal?))
		(when foreground? (filter-foreground! manager keep?))
		(when background? (filter-background! manager keep?))
	)

	(define (do! manager program [stack? #f]) ; Load a foreground program. If stack? the current program will be stacked.
		(define (keep? p) (not (ai-program-equal? p program)))
		(filter-background! manager keep?) ; Remove existing from background set.

		(ai-program-free! (active manager)) ; Execute current program destructor.
		(set-ai-manager-main! manager (cons program
			(let ((main (ai-manager-main manager)))
				(filter keep? ; Remove existing from foreground set.
					(if (or (null? main) stack?) main (cdr main)) ; Remove current program if not stack?.
				)
			)
		))
		(ai-program-load! program) ; Execute loading program constructor.
	)

	(define (run! manager event connection) ; Execute iterations of all active programs.
		; TODO triggers

		; Execute background programs.
		(set-ai-manager-todo! manager (append ; Reset iterator.
			(reverse (ai-manager-done manager))
			(ai-manager-todo manager)
		)) (set-ai-manager-done! manager (list))

		(define free (list))
		(apply free! manager (do ((todo (ai-manager-todo manager) (ai-manager-todo manager))) ((null? todo) free)
			(let ((p (car todo)))
				(set-ai-manager-todo! manager (cdr todo))
				(set-ai-manager-done! manager (cons p (ai-manager-done manager)))

				(when (finished? (ai-program-run! p event connection))
					(set! free (cons p free))
				)
			)
		))

		; Execute foreground program.
		(let ((a (active manager)))
			(when (finished? (ai-program-run! a event connection))
				(free! manager a)
			)
		)
	)
)
