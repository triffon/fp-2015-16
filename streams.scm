(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))

(define error (delay (car '())))

(define undefined (delay (+ a 3)))
(define a 10)

;(define (mydelay x) (lambda () x))
(define-syntax mydelay
  (syntax-rules ()
    ((delay x) (lambda () x))))
(define (myforce promise) (promise))

(define the-empty-stream '())
;(define (cons-stream h t) (cons h (delay t)))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t) (cons h (delay t)))))

(define head car)
(define (tail s) (force (cdr s)))
(define empty-stream? null?)

(define s (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))

(define (enum a b)
  (if (> a b)
      the-empty-stream
      (cons-stream a (enum (+ a 1) b))))

(define (first n s)
  (if (or (empty-stream? s) (= n 0)) '()
      (cons (head s) (first (- n 1) (tail s)))))

(define (search-stream p? s)
  (cond ((empty-stream? s) #f)
        ((p? (head s)) s)
        (else (search-stream p? (tail s)))))


(define (from n)
  (cons-stream n (from (+ n 1))))

(define nats (from 0))

(define (fibs-from current prev)
   (cons-stream current (fibs-from (+ current prev) current)))

(define fibs (cons-stream 0 (fibs-from 1 0)))

(define (map-stream f s)
  (cons-stream (f (head s)) (map-stream f (tail s))))

(define (filter-stream p? s)
  (if (p? (head s))
      (cons-stream (head s) (filter-stream p? (tail s)))
      (filter-stream p? (tail s))))

(define evens (filter-stream even? nats))

(define (zip-streams op s1 s2)
  (cons-stream (op (head s1) (head s2))
               (zip-streams op (tail s1) (tail s2))))

(define evens (zip-streams + nats nats))

(define (gen-ones)
  (cons-stream 1 (gen-ones)))

(define ones (cons-stream 1 ones))

(define nats (cons-stream 0 (zip-streams + ones nats)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (zip-streams + fibs (tail fibs)))))

(define (notdivides p)
  (lambda (x) (> (remainder x p) 0)))

(define (sieve s)
  (cons-stream (head s)
                (sieve
                  (filter-stream (notdivides (head s)) (tail s)))))

(define (sieve2 s)
  (cons-stream (head s)
                  (filter-stream (notdivides (head s)) (sieve2 (tail s)))))


(define primes (sieve (from 2)))