(module script racket/base
	(require
		(only-in srfi/1 car+cdr fold first second third fourth split-at)
		racket/contract
		(only-in racket/string string-replace string-split string-trim)
		(relative-in "../."
			"library/extension.scm"
			"system/structure.scm"
			"model/creature.scm"
			"model/character.scm"
			"model/protagonist.scm"
		)
	)
	(provide (contract-out
		(evaluate (->* (string?) #:rest (listof pair?) any))
	))

	(define (tank-class? character) ; TODO in class & has shield
		(member (ref character 'name) (list "sidus" "fury" "evdem" "ekon") string-ci=?)
	)
	(define (wizard-class? character)
		(member (ref character 'class) (list
			'human-wizard 'sorcerer 'archmage
			'elven-wizard 'spellsinger 'mystic-muse
			'dark-wizard 'spellhowler 'storm-screamer
		) eq?)
	)
	(define (empty-slot? creature slot)
		(let ((item-id (ref creature 'clothing (string->symbol slot))))
			(or (not item-id) (zero? item-id))
		)
	)

	(define symbols (list
		(list "fighter"	1	character?		fighter-type?)
		(list "mystic"		1	character?		mystic-type?)

		(list "support"	1	character?		support-class?)
		(list "wizard"		1	character?		wizard-class?)
		(list "tank"		1	character?		tank-class?)

		(list "human"		1	character?		(lambda (for) (eq? (ref for 'race) 'human)))
		(list "elf"			1	character?		(lambda (for) (eq? (ref for 'race) 'light-elf)))
		(list "dark"		1	character?		(lambda (for) (eq? (ref for 'race) 'dark-elf)))
		(list "orc"			1	character?		(lambda (for) (eq? (ref for 'race) 'orc)))
		(list "dwarf"		1	character?		(lambda (for) (eq? (ref for 'race) 'dwarf)))

		(list "wounded"	1	creature?		(lambda (for) (<= (hp-ratio for) 97/100)))
		(list "tired"		1	creature?		(lambda (for) (<= (mp-ratio for) 97/100)))
		(list "casting"	1	creature?		casting?)
		(list "sit"			1	creature?		(lambda (for) (not (standing? for))))
		(list "dead"		1	creature?		(lambda (for) (ref for 'alike-dead?)))

		(list "level"		1	creature?		(lambda (for) (ref for 'level)))
		(list "hp"			1	creature?		(lambda (for) (ref for 'hp)))
		(list "mp"			1	creature?		(lambda (for) (ref for 'mp)))
		(list "pvp"			1	protagonist?	(lambda (for) (ref for 'pvp-count)))
		(list "pk"			1	protagonist?	(lambda (for) (ref for 'pk-count)))

		; (list "party"	1	protagonist?	(lambda (for party) ...)) ; TODO
		; (list "leader"	1	protagonist?	(lambda (for party) ...)) ; TODO
		(list "name"		1	creature?		(lambda (for v) (string-ci=? (ref for 'name) v)))
		(list "empty"		1	creature?		empty-slot?)
		(list "class"		1	character?		(lambda (for v) (string-ci=? (symbol->string (ref for 'class)) v)))

		(list "="			2	#f					(lambda (a b) (equal? a b)))
		(list ">"			2	#f					(lambda (a b) (> a b)))
		(list "<"			2	#f					(lambda (a b) (< a b)))
		(list ">="			2	#f					(lambda (a b) (>= a b)))
		(list "<="			2	#f					(lambda (a b) (<= a b)))

		(list "not"			3	#f					(lambda (a) (not a)))
		(list "and"			4	#f					(lambda (a b) (and a b)))
		(list "or"			4	#f					(lambda (a b) (or a b)))
	))

	(define (priority f)
		(let ((d (assoc f symbols string=?)))
			(and d (second d))
		)
	)
	(define (arity f)
		(let ((d (assoc f symbols string=?)))
			(- (procedure-arity (fourth d)) (if (third d) 1 0))
		)
	)
	(define (has-context? f)
		(third (assoc f symbols string=?))
	)
	(define (call f context args)
		(let* ((d (assoc f symbols string=?)) (check (third d)))
			(if check
				(and (check context) (apply (fourth d) context args))
				(apply (fourth d) args)
			)
		)
	)
	(define (numeric-constant? v)
		(regexp-match? #rx"^[0-9]+$" v)
	)
	(define (string-constant? v)
		(and (string-starts? v "\"") (string-ends? v "\""))
	)

 	; TODO contextual and/or?
	; TODO string constants without quotes?
	(define (error-no-symbol) (error "Symbol not exists."))
	(define (error-no-argument) (error "Not enough arguments."))
	(define (prepare left [r (list)])
		(if (not (null? left))
			(let-values (((lex left) (car+cdr left))) (cond
				((string=? lex "(") (let-values (((nr left) (prepare left))) (prepare left (cons nr r))))
				((string=? lex ")") (values (reverse r) left))
				((numeric-constant? lex) (prepare left (cons (string->number lex) r)))
				((string-starts? lex ".") (prepare left (cons (substring lex 1) (cons "party" r)))) ; In party.
				(else (prepare left (cons lex r)))
			))
			(values (reverse r) left)
		)
	)
	(define (p queue bindings left [r (list)])
		(if (not (null? left))
			(let-values (((lex left) (car+cdr left))) (cond
				((list? lex) ; Nesting.
					(p queue bindings left (cons (p queue bindings lex) r))
				)
				((and (string? lex) (not (string-constant? lex)) (not (member lex bindings string=?))) ; Operator.
					(let ((pr (priority lex))) (cond
						((not pr) (error-no-symbol)) ; TODO context propagation
						((= pr queue) (let ((ar (arity lex)))
							(if (= ar 2) ; Binary operator.
								(if (and (not (null? r)) (not (null? left)))
									(let ((n (if (list? (car left)) (p queue bindings (car left)) (car left))))
										(p queue bindings (cdr left) (cons (list lex (car r) n) (cdr r)))
									)
									(error-no-argument)
								)
								(let ((context (and (has-context? lex) (not (null? left)) (string? (car left)) (member (car left) bindings) (car left))))
									(let-values (((args left) (split-at (if context (cdr left) left) ar))) ; TODO (error-no-argument)
										(let ((ops (if (has-context? lex) (cons (or context (car bindings)) args) args)))
											(p queue bindings left (cons (cons lex ops) r))
										)
									)
								)
							)
						))
						(else (p queue bindings left (cons lex r)))
					))
				)
				(else (p queue bindings left (cons lex r))) ; Constant.
			))
			(reverse r)
		)
	)
	(define (parse expr bindings)
		(let ((lexemes (string-split (regexp-replace* #rx"(<=|>=|[=><()])" (string-downcase expr) " \\1 "))))
			(let-values (((raw left) (prepare lexemes)))
				(fold (lambda (q r) (p q bindings r)) raw (list 1 2 3 4))
			)
		)
	)

	(define (error-idle) (error "Nothing to do."))
	(define (error-ambiguity) (error "Value is ambiguous."))
	(define (parse-layer layer bindings)
		(let-values (((fn args) (car+cdr layer)))
			(if (has-context? fn)
				(values fn (cdr (assoc (car args) bindings string=?)) (cdr args))
				(values fn #f args)
			)
		)
	)
	(define (e layer bindings)
		(cond
			((null? layer)
				(error-idle)
			)
			((list? (car layer)) ; Nesting.
				(if (null? (cdr layer))
					(e (car layer) bindings)
					(error-ambiguity)
				)
			)
			((or (number? (car layer)) (string-constant? (car layer)))
				(if (null? (cdr layer))
					(car layer)
					(error-ambiguity)
				)
			)
			; ((string-constant? (car layer))
			; 	(if (null? (cdr layer))
			; 		(call "name" (cdar bindings) (list (string-trim (car layer) "\"")))
			; 		(error-ambiguity)
			; 	)
			; )
			(else (let-values (((fn context args) (parse-layer layer bindings)))
				(if (= (length args) (arity fn))
					(call fn context (map (lambda (arg) (cond
						((null? arg) (error-idle))
						((list? arg) (e arg bindings))
						((number? arg) arg)
						((string-constant? arg) (string-trim arg "\""))
						(else (error "Argument not valid."))
					)) args))
					(error-ambiguity)
				)
			))
		)
	)
	(define (evaluate expr . bindings)
		(let ((ast (parse expr (map car bindings))))
			(e ast bindings)
		)
	)
)
