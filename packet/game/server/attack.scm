(module packet racket/base
	(require "../../packet.scm")
	(provide game-server-packet/attack)
	
	(define (read-hit s)
		(let ((target-id (read-int32 #f s)) (damage (read-int32 #t s)) (flags (read-byte s)))
			(list
				(cons 'target-id target-id)
				(cons 'damage damage)			
				(cons 'soulshot? (= (bitwise-and flags #x10) #x10))
				(cons 'critical? (= (bitwise-and flags #x20) #x20))
				(cons 'shield? (= (bitwise-and flags #x40) #x40))
				(cons 'miss? (= (bitwise-and flags #x80) #x80))
				(cons 'grade (list-ref
					(list 'ng 'd 'c 'b 'a 's)
					(bitwise-bit-field flags 0 4)
				))
			)
		)
	)
	
	(define (read-hits s c n l)
		(if (< n c)
			(let ((i (read-hit s)))
				(read-hits s c (+ n 1) (cons i l))
			)
			l
		)
	)
	
	(define (game-server-packet/attack buffer)
		(let ((s (open-input-bytes buffer)))
			(let ((id (read-byte s)) (object-id (read-int32 #f s)) (hit (read-hit s)))
				(list
					(cons 'id id)
					(cons 'object-id object-id)					
					(cons 'position (read-point s))
					(cons 'hits (cons hit
						(read-hits s (read-int16 #f s) 0 (list))
					))
				)
			)
		)
	)
)


		#|
		writeC(0x05);

		writeD(_attackerId);
		writeD(hits[0]._targetId);
		writeD(hits[0]._damage);
		writeC(hits[0]._flags);
		writeD(_x);
		writeD(_y);
		writeD(_z);
		writeH(hits.length-1);
		for (int i=1; i < hits.length; i++)
		{
			writeD(hits[i]._targetId);
			writeD(hits[i]._damage);
			writeC(hits[i]._flags);
		}
		
		
            if (_soulshot) _flags |= 0x10 | _grade;
            if (crit)      _flags |= 0x20;
            if (shld)      _flags |= 0x40;
            if (miss)      _flags |= 0x80;
			
			
	public static final int CRYSTAL_NONE = 0x00; // ??
	public static final int CRYSTAL_D = 0x01; // ??
	public static final int CRYSTAL_C = 0x02; // ??
	public static final int CRYSTAL_B = 0x03; // ??
	public static final int CRYSTAL_A = 0x04; // ??
	public static final int CRYSTAL_S = 0x05; // ??
		|#