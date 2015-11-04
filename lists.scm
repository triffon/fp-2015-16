(define l '(1 2 3 4 5 6 7))

(define (length l)
  (if (null? l) 0
      (+ 1 (length (cdr l)))))

(define (list-tail l n)
  (if (= n 0) l
      (list-tail (cdr l) (- n 1))))

(define (list-ref l n)
  (car (list-tail l n)))

(define (member x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) l)
        (else (member x (cdr l)))))


(define (from-to a b)
  (if (> a b) '()
      (cons a (from-to (+ a 1) b))))

(define (collect a b next)
  (if (> a b) '()
      (cons a (collect (next a) b next))))

(define (square x) (* x x))

(define (append l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append (cdr l1) l2))))

(define (reverse l)
  (define (iter l result)
    (if (null? l) result
        (iter (cdr l) (cons (car l) result))))
  (iter l '()))

(define (rcons x l)
  (append l (list x)))

(define (reverse l)
  (if (null? l) '()
      (rcons (car l) (reverse (cdr l)))))

(define (twice f)
  (lambda (x) (f (f x))))

(define (1+ n) (+ n 1))

(map (lambda (f) (f 2)) (list square 1+ odd?))

(map (lambda (f) (f 2)) (map twice (list square 1+ boolean?)))

(define (reverse l)
  (foldr rcons '() l))

(define (map f l)
  (foldr (lambda (h t) (cons (f h) t)) '() l))

(define (filter p? l)
  (foldr (lambda (h t) (if (p? h) (cons h t) t)) '() l))

(define (accumulate op nv a b term next)
  (foldr op nv (map term (collect a b next))))
