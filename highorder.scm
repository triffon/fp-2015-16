(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (square x) (* x x))
(define (1+ x) (+ x 1))
; (accumulate square 0 1 5 sin 1+)
(accumulate + 0 1 5 * 1+)

(define (my* a b) (* a b))

; (accumulate + 0 1 5 my* 1+)

(define (p n x)
  (define (term i) (* (- (1+ n) i) (expt x i)))
  (accumulate + 0 0 n term 1+))

(p 4 2)

(define (id x) x)

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))



(define (p n x)
  (define (op a b) (+ (* a x) b))
  (accumulate-i op 0 1 (1+ n) id 1+))

(p 4 2)

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (fact n)
  (accumulate * 1 1 n id 1+))
(fact 5)

(define (pow x n)
  (accumulate * 1 1 n (lambda (i) x) 1+))
(pow 2 6)

(define (myexp x n)
  (accumulate + 0 0 n (lambda (i) (/ (pow x i) (fact i))) 1+))

(myexp 2.5 100)
(exp 2.5)

(define (myexp x n)
  (accumulate + 0 0 n (lambda (i) (/
                                   (accumulate * 1 1 i (lambda (j) x) 1+)
                                   (accumulate * 1 1 i id 1+)))

              1+))

(myexp 2.5 100)
;  (accumulate op nv a b term next))

(define (exists? p a b)
  (accumulate (lambda (x y) (or x y)) #f a b p 1+))

(exists? odd? 1 5)

(define (repeated f n)
  (lambda (x) (if (= n 0) x (f ((repeated f (- n 1)) x)))))

(define 10+ (repeated 1+ 10))
(10+ 5)

(define (compose f g) (lambda (x) (f (g x))))

((repeated square 1) 124)

(define (repeated f n)
  (accumulate compose id 1 n (lambda (i) f) 1+))
; (accumulate *       1  1 n (lambda (i) x) 1+))

(define 10+ (repeated 1+ 10))
(10+ 5)

(define (derive f dx) (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (derive-n f n dx)
  ((repeated (lambda (f) (derive f dx)) n) f))

((derive-n square 2 0.0001) 10)

(define Y (lambda (f)
            ((lambda (x) (f (lambda (n) ((x x) n))))
             (lambda (x) (f (lambda (n) ((x x) n)))))))
(define fact-body (lambda (f)
                    (lambda (n) (if (= n 0) 1
                                    (* n (f (- n 1)))))))
(define fact (Y fact-body))
(fact 5)
