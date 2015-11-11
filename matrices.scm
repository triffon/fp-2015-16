(define m '((1 2 3) (4 5 6)))

(define get-rows length)
(define (get-columns m) (length (car m)))

(define (get-first-row m) (car m))
(define (get-first-column m) (map car m))

(define del-first-row cdr)
(define (del-first-column m) (map cdr m))

(define (get-row i m) (list-ref m i))
(define (get-column i m)
   (map (lambda (row) (list-ref row i)) m))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (1+ x) (+ x 1))

(define (transpose m)
  (accumulate cons '() 0 (- (get-columns m) 1)
              (lambda (i) (get-column i m)) 1+))

(define (transpose m) (apply map list m))

(define (sum-vectors v1 v2) (map + v1 v2))
(define (sum-matrices m1 m2) (map sum-vectors m1 m2))

(define (mult-vectors v1 v2)
  (apply + (map * v1 v2)))

(define (mult-matrices m1 m2)
  (let ((m2t (transpose m2)))
    (map (lambda (row)
           (map (lambda (column) (mult-vectors row column))
                m2t))
         m1)))
