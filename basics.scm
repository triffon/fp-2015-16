(define x 5)
(define x 10)
(define (g x) (- (g (+ x 1)) 1))
(define (h) (+ 2 3))
(define (sign x)
  (if (> x 0) 1
      (if (< x 0) -1 0)))

(define (f x)
  (if (> x (if (odd? x) 2 3)) 8 10))

(define (f2 x)
  (if ((if (odd? x) > <) x 3) 11 14))
