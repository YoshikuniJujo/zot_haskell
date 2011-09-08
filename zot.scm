(define S (lambda (x) (lambda (y) (lambda (z) ((x z)(y z))))))
(define K (lambda (x) (lambda (y) x)))
(define I (lambda (x) x))
(define zero (lambda (c) (c (lambda (f) ((f S) K)))))
(define one (lambda (c) (lambda (L)
			  (L (lambda (l) (lambda (R)
					   (R (lambda (r) (c (l r))))))))))

; interrogate: ^f.fIIIK: maps zero onto K, one onto KI, and output onto K(KI).
(define interrogate (lambda (f) ((((f I) I) I) K)))
(define output (K (K (K (K (K (K I)))))))

(define print 
  ((lambda (x) (x x))
   (lambda (self)
     (lambda (c)
       (display (((interrogate c) "0") "1"))
       (self self)))))

(do ((v (lambda (c) (c I)) (if (eq? #\0 (read-char)) (v zero) (v one))))
    ((eof-object? (peek-char)) ((v output) print)))
