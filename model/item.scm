(module logic racket/base
	(require
		(only-in srfi/1 fold alist-delete)
		(only-in racket/function negate)
		racket/contract
		"../library/geometry.scm"
		"../system/structure.scm"
		"../system/database.scm"
		"object.scm"
	)

	(provide (contract-out
		(item? (-> any/c boolean?))
		(on-ground? (-> item? boolean?))
		(in-inventory? (-> item? boolean?))
		(make-item (-> list? any/c box?))
		(update-item! (-> box? list? list?))

		(weapon? (-> item? boolean?))
		(shield? (-> item? boolean?))
		(armor/heavy? (-> item? boolean?))
		(armor/light? (-> item? boolean?))
		(armor/robe? (-> item? boolean?))
		(armor? (-> item? boolean?))
		(jewelry? (-> item? boolean?))
		(pet? (-> item? boolean?))
		(usable? (-> item? boolean?))
		(equippable? (-> item? boolean?))
		(stackable? (-> item? boolean?))
		(crystallizable? (-> item? boolean?))
	))

	(define item (list
		(cons 'item-id (negate =))
		(cons 'position (negate point/3d=?))
		(cons 'count (negate =))
		(cons 'enchant (negate =))
		(cons 'quest? (negate eq?))

		(cons 'name #f)
		(cons 'item-type #f)
		(cons 'grade #f)
		(cons 'stack #f)
	))

	(define (item? object)
		(object-of-type? object 'item)
	)

	(define (on-ground? item)
		(if (ref item 'position) #t #f)
	)

	(define (in-inventory? item)
		(not (on-ground? item))
	)

	(define (make-item data db) ; TODO Can be optimized.
		(let* ((object (make-object data)) (type (cons 'item (ref object 'type))))
			(box (append
				(list (cons 'type type))
				(db-item db (ref data 'item-id))
				(fold ; TODO extract item-id
					(lambda (p r) (if (and p (assoc (car p) item eq?)) (cons p r) r)) ; If field belongs to item.
					(alist-delete 'type object) ; TODO extract type
					data
				)
			))
		)
	)

	(define (update-item object data)
		(struct-update data item (update-object object data))
	)
	(define (update-item! object data)
		(let-values (((rest updated changes) (update-item (unbox object) data)))
			(set-box! object (append rest updated))
			changes
		)
	)

	(define (weapon? item)
		(if (member (ref item 'item-type) (list 'sword 'blunt 'dagger 'spear 'bow 'duals 'fists 'wand) eq?) #t #f)
	)
	(define (shield? item)
		(eq? (ref item 'item-type) 'shield)
	)
	(define (armor/heavy? item)
		(if (member (ref item 'item-type) (list 'full/heavy 'body/heavy 'legs/heavy) eq?) #t #f)
	)
	(define (armor/light? item)
		(if (member (ref item 'item-type) (list 'full/light 'body/light 'legs/light) eq?) #t #f)
	)
	(define (armor/robe? item)
		(if (member (ref item 'item-type) (list 'full/robe 'body/robe 'legs/robe) eq?) #t #f)
	)
	(define (armor? item)
		(if (or
			(armor/heavy? item) (armor/light? item) (armor/robe? item)
			(member (ref item 'item-type) (list 'helmet 'gloves 'boots) eq?)
		) #t #f)
	)
	(define (jewelry? item)
		(if (member (ref item 'item-type) (list 'necklace 'earring 'ring) eq?) #t #f)
	)
	(define (pet? item)
		(if (member (ref item 'item-type) (list 'wolf 'dragon 'strider 'wyvern 'kookaburra 'buffalo 'cougar 'redeemer) eq?) #t #f)
	)
	(define (usable? item)
		(if (or
			(member (ref item 'item-type) (list 'potion 'scroll 'recipe 'enchant/weapon 'enchant/armor) eq?)
			(pet? item)
		) #t #f)
	)
	(define (equippable? item)
		(if (or
			(weapon? item) (shield? item) (armor? item)
			(member (ref item 'item-type) (list 'hair 'arrow) eq?)
		) #t #f)
	)
	(define (stackable? item)
		(let ((stack (ref item 'stack)))
			(or (> stack 1) (zero? stack))
		)
	)
	(define (crystallizable? item)
		(> (ref item 'grade) 0)
	)
)
