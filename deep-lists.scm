(define (atom? x) (not (pair? x)))

(define l '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

(define (count-atoms l)
   (cond ((null? l) 0)
         ((atom? l) 1)
         (else (+ (count-atoms (car l)) (count-atoms (cdr l))))))

(define (flatten l)
  (cond ((null? l) '())
        ((atom? l) (list l))
        (else (append (flatten (car l)) (flatten (cdr l))))))

(define (rcons x l)
   (append l (list x)))

(define (deep-reverse l)
  (cond ((null? l) '())
        ((atom? l) l)
        (else (rcons (deep-reverse (car l)) (deep-reverse (cdr l))))))

(define (deep-fold nv term op l)
  (cond ((null? l) nv)
        ((atom? l) (term l))
        (else (op (deep-fold nv term op (car l))
                  (deep-fold nv term op (cdr l))))))

(define (count-atoms l) (deep-fold 0 (lambda (x) 1) + l))

(define (flatten l) (deep-fold '() list append l))

(define (rcons x l) (append l (list x)))
(define (deep-reverse l) (deep-fold '() id rcons l))

(define (branch p? f g) (lambda (x) (p? x) (f x) (g x)))
(define (deep-fold nv term op l)
  (foldr op nv
   (map (branch atom?
                term
                (lambda (l) (deep-fold nv term op l)))
        l)))
