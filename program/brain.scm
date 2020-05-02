(module ai racket/base
	(require
		srfi/1
		"program.scm"
		(relative-in "../."
			"library/extension.scm"
			"system/debug.scm"
		)
	)
	(provide
		make-brain
		brain-active
		brain-count
		brain-run!
		brain-do!
		brain-load!
		brain-stop!
		brain-clear!
	)

	(struct brain (
			connection
			default ; Defaunt program.
			[main #:auto #:mutable] ; Foreground programs stack.
			[todo #:auto #:mutable] ; Background programs set, part 1.
			[done #:auto #:mutable] ; Background programs set, part 2.
			; [triggers #:auto #:mutable] ; Triggers set.
		)
		#:constructor-name make-brain
		#:auto-value (list)
	)

	(define (brain-active brain) ; Get active foreground program (or default).
		(try-first (brain-main brain) (brain-default brain))
	)
	(define (brain-count brain) ; Total programs count.
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

	(define (log-program-error e)
		(apf-warn "Program \"~a\" error: ~a" (exn:error:program-id e) (exn-message e))
	)

	(define (brain-load! brain . programs) ; Load background programs.
		(define cn (brain-connection brain))
		(set-brain-todo! brain (append (brain-todo brain) (filter (lambda (p)
			(with-handlers ((exn:error:program?
				(lambda (e)
					(log-program-error e)
					#f ; Don't add if loaded with error.
				)))
				(and
					(not (exists? brain p)) ; Don't replace existsing (use free before).
					(program-load! cn p) ; Execute loading program constructor and add if should be.
				)
			)
		) programs)))
	)

	(define (chain-load! cn def lst)
		(let ((p (if (null? lst) def (car lst))))
			(with-handlers ((exn:error:program?
				(lambda (e)
					(log-program-error e)
					(chain-load! cn def (cdr lst)) ; Try next program on error.
				)))
				(if (program-load! cn p) ; Execute loading or default program constructor.
					lst ; Return new foreground stack.
					(chain-load! cn def (cdr lst)) ; Try next program if this can't be loaded.
				)
			)
		)
	)
	(define (filter-foreground! brain keep?)
		(let ((cn (brain-connection brain)) (main (brain-main brain)))
			(when (not (null? main))
				(let ((s (filter keep? (cdr main))))
					(set-brain-main! brain
						(if (not (keep? (car main))) ; If active program should be unloaded.
							(begin
								(program-free! cn (car main)) ; Execute current program destructor.
								(chain-load! cn (brain-default brain) s) ; Execute unstacking program constructor.
							)
							(cons (car main) s) ; Else just prepend to filtered stack.
						)
					)
				)
			)
		)
	)

	(define (filter-background! brain keep?)
		(define cn (brain-connection brain))
		(define (keep-or-destruct p) (or (keep? p) (begin (program-free! cn p) #f)))
		(set-brain-todo! brain (filter keep-or-destruct (brain-todo brain)))
		(set-brain-done! brain (filter keep-or-destruct (brain-done brain)))
	)

	(define (brain-stop! brain . programs) ; Unload amount of programs.
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
		(define cn (brain-connection brain))
		(define (keep? p) (not (program-equal? p program)))

		(filter-background! brain keep?) ; Remove existing from background set.
		(program-free! cn (brain-active brain)) ; Execute current program destructor.
		(set-brain-main! brain (chain-load! cn (brain-default brain) ; Drop programs from foreground stack till first doable.
			(cons program (let ((main (brain-main brain))) ; Remove same as loading programs from foreground stack.
				(filter keep? (if (or stack? (null? main)) main (cdr main))) ; Drop also current program if not stack?.
			))
		))
	)

	(define (raise-ambiguous-main . causers)
		(apply raise-user-error "Foreground program is ambiguous, causers: " (map program-id causers))
	)

	(define (program-log-run! cn p ev)
		(with-handlers ((exn:error:program?
			(lambda (e)
				(log-program-error e)
				#f ; Consider program finished on error.
			)))
			(program-run! cn p ev)
		)
	)
	(define (bg-filter-run! br fn)
		(set-brain-todo! br (append ; Rewind iterator.
			(reverse (brain-done br))
			(brain-todo br)
		)) (set-brain-done! br (list))

		(do ((todo (brain-todo br) (brain-todo br))) ((null? todo)) ; Handle programs.
			(set-brain-todo! br (cdr todo))
			(when (fn (car todo)) ; Drop program from queue if not continue?.
				(set-brain-done! br (cons (car todo) (brain-done br)))
			)
		)
	)
	(define (brain-run! br ev) ; Execute iterations of all active programs.
		(define cn (brain-connection br))
		(define active-changer #f)

		; Execute background programs.
		(bg-filter-run! br (lambda (p)
			(let* ((a (brain-active br)) (continue? (program-log-run! cn p ev)))
				(when (not (eq? a (brain-active br))) ; If foreground program was changed during program iteration.
					(if (not active-changer) ; Error if foreground program was changed at least twice.
						(set! active-changer p)
						(raise-ambiguous-main active-changer p)
					)
				)
				(when (not continue?)
					(program-free! cn p) ; Unload program.
				)
				continue?
			)
		))

		; Execute foreground program.
		(let ((p (brain-active br)))
			(when (not (program-log-run! cn p ev))
				(brain-stop! br p)
			)
		)
	)
)
