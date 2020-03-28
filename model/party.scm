(module logic racket/base
	(require
		(rename-in racket/contract (any all/c))
	)
	(provide (contract-out
		(make-party (->* (symbol? integer?) #:rest (listof integer?) list?))
		(in-party? (->* ((or/c list? false/c)) (integer?) boolean?))
		(party-members (-> list? (listof integer?)))
		(party-leader (-> list? (or/c integer? false/c)))
		(party-loot (-> list? symbol?))
	))

	(define (make-party loot-mode leader-id . object-ids)
		(apply list loot-mode leader-id object-ids)
	)

	(define (in-party? party [object-id #f])
		(cond
			((not party) #f) ; Party empty.
			(object-id (member object-id (cdr party) =)) ; Member exists.
			(else #t) ; I'm in party.
		)
	)

	(define (party-members party)
		(if party (cdr party) (list))
	)

	(define (party-leader party)
		(if party (car (cdr party)) #f)
	)

	(define (party-loot party)
		(if (in-party? party) (car party) #f)
	)
)
