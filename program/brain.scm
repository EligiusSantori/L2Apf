(module ai racket/base
	(require
		srfi/1
		(only-in "program.scm"
			program-id
			program-equal?
			program-load!
			program-free!
			program-run!
		)
		(relative-in "../.."
			"library/extension.scm"
		)
	)
	(provide
		make-brain
		brain-run!
		brain-do!
		brain-load!
		brain-free!
		brain-clear!
	)

	(struct brain (
			default ; Defaunt program.
			[main #:auto #:mutable] ; Foreground programs stack.
			[todo #:auto #:mutable] ; Background programs set, part 1.
			[done #:auto #:mutable] ; Background programs set, part 2.
			; [triggers #:auto #:mutable] ; Triggers set.
		)
		#:constructor-name make-brain
		#:auto-value (list)
	)

	(define (active brain) ; Get active foreground program (or default).
		(try-first (brain-main brain) (brain-default brain))
	)
	(define (count brain) ; Total programs count.
		(+
			(length (brain-main brain))
			(length (brain-done brain))
			(length (brain-todo brain))
		)
	)
	(define (foreground? brain program) ; If program exists in foreground stack.
		(member program (brain-main brain) program-equal?)
	)
	(define (background? brain program) ; If program exists in background set.
		(or
			(member program (brain-done brain) program-equal?)
			(member program (brain-todo brain) program-equal?)
		)
	)
	(define (exists? brain program) ; If program exists in brain.
		(or
			(foreground? brain program)
			(background? brain program)
		)
	)

	(define (brain-load! brain . programs) ; Load background programs.
		(set-brain-todo! brain
			(append (brain-todo brain) (filter (lambda (p)
				(and (not (exists? brain p)) ; Don't replace existsing (use free before).
					(begin (program-load! p) p) ; Execute loading program constructor.
				)
			) programs))
		)
	)

	(define (filter-foreground! brain keep?)
		(let ((main (brain-main brain))) (when (not (null? main))
			(let ((s (filter keep? (cdr main))))
				(set-brain-main! brain
					(if (not (keep? (car main))) ; If active program should be unloaded.
						(begin
							(program-free! (car main)) ; Execute current program destructor.
							(program-load! (try-first s (brain-default brain))) ; Execute unstacking program constructor.
							s
						)
						(cons (car main) s) ; Else just prepend to filtered stack.
					)
				)
			)
		))
	)

	(define (filter-background! brain keep?)
		(define (keep?+ p) (or (keep? p) (begin (program-free! p) #f)))
		(set-brain-todo! brain (filter keep?+ (brain-todo brain)))
		(set-brain-done! brain (filter keep?+ (brain-done brain)))
	)

	(define (brain-free! brain . programs) ; Unload amount of programs.
		(define (keep? p) (not (member p programs program-equal?)))
		(when (not (null? programs))
			(filter-foreground! brain keep?)
			(filter-background! brain keep?)
		)
	)

	(define (brain-clear! brain [foreground? #t] [background? #f] [except (list)]) ; Unload all foreground or/and background programs.
		(define (keep? p) (member p except program-equal?))
		(when foreground? (filter-foreground! brain keep?))
		(when background? (filter-background! brain keep?))
	)

	(define (brain-do! brain program [stack? #f]) ; Load a foreground program. If stack? the current program will be stacked.
		(define (keep? p) (not (program-equal? p program)))
		(filter-background! brain keep?) ; Remove existing from background set.

		(program-free! (active brain)) ; Execute current program destructor.
		(set-brain-main! brain (cons program
			(let ((main (brain-main brain)))
				(filter keep? ; Remove existing from foreground stack.
					(if (or (null? main) stack?) main (cdr main)) ; Remove current program if not stack?.
				)
			)
		))
		(program-load! program) ; Execute loading program constructor.
	)

	(define (brain-run! brain event connection) ; Execute iterations of all active programs.
		(define turns (list)) ; Background programs which changed foreground program.
		(define (log-turns! program) ; TODO Выбрасывать исключение по ходу. Избавиться от turns.
			(let ((a (active brain)))
				(let ((r (program-run! program event connection))) ; Executer background program iterator.
					(when (not (eq? a (active brain))) ; If foreground program was changed during program iteration.
						(set! turns (cons program turns))
					)
					r
				)
			)
		)

		; TODO triggers

		; Execute background programs.
		(set-brain-todo! brain (append ; Rewind iterator.
			(reverse (brain-done brain))
			(brain-todo brain)
		)) (set-brain-done! brain (list))

		(do ((todo (brain-todo brain) (brain-todo brain))) ((null? todo)) ; Run programs.
			(let ((p (car todo)))
				(set-brain-todo! brain (cdr todo))
				(if (log-turns! p) ; If program has not finished.
					(set-brain-done! brain (cons p (brain-done brain))) ; Keep in queue.
					(program-free! p) ; Unload & drop program from queue.
				)
			)
		)

		; Execute foreground program.
		(when (length> turns 1) ; If foreground program was changed twice or more.
			(apply raise-user-error "Foreground program is ambiguous, causers:" (map program-id turns))
		)

		(let ((a (active brain)))
			(when (not (program-run! a event connection))
				(brain-free! brain a)
			)
		)
	)
)
