(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
	)
	(provide (contract-out
		(make-party (->* (symbol? integer?) #:rest (listof integer?) list?))
		(in-party? (-> (or/c list? false/c) integer? boolean?))
		(party-members (-> (or/c list? false/c) (listof integer?)))
		(party-leader (-> (or/c list? false/c) (or/c integer? false/c)))
		(party-loot (-> (or/c list? false/c) (or/c symbol? false/c)))
	))

	(define (make-party loot-mode leader-id . object-ids)
		(apply list loot-mode leader-id object-ids)
	)

	(define (in-party? party object-id)
		(if (and party (member object-id (cdr party) =)) #t #f)
	)

	(define (party-members party)
		(if party (cdr party) (list))
	)

	(define (party-leader party)
		(and party (cadr party))
	)

	(define (party-loot party)
		(and party (car party))
	)
)
