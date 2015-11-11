; директна реализация на базовите операции

(define make-rat cons)
(define get-numer car)
(define get-denom cdr)

; защита срещу нулев знаменател
(define (make-rat n d)
   (if (= d 0) (cons n 1) (cons n d)))

; аритметични операции
(define (*rat p q)
  (make-rat (* (get-numer p) (get-numer q))
            (* (get-denom p) (get-denom q))))

(define (+rat p q)
  (make-rat (+ (* (get-numer p)
                  (get-denom q))
               (* (get-denom p)
                  (get-numer q)))
            (* (get-denom p) (get-denom q))))

(define (<rat p q)
  (< (* (get-numer p) (get-denom q))
     (* (get-numer q) (get-denom p))))

; нормализирани дроби
(define (make-rat n d)
  (if (or (= d 0) (= n 0)) (cons 0 1)
    (let* ((g (gcd n d))
           (ng (quotient n g))
           (dg (quotient d g)))
       (if (> dg 0) (cons ng dg)
                    (cons (- ng) (- dg))))))

; представяне със сигнатура
(define (make-rat n d)
  (cons 'rat
        (if (or (= d 0) (= n 0)) (cons 0 1)
            (let* ((g (gcd n d))
                   (ng (quotient n g))
                   (dg (quotient d g)))
              (if (> dg 0) (cons ng dg)
                  (cons (- ng) (- dg)))))))

; проверка на сигнатурата
(define (rat? p)
  (and (pair? p) (eq? (car p) 'rat)
       (pair? (cdr p)) (number? (cadr p)) (number? (cddr p))))

; защитени селектори
(define (check-rat f)
  (lambda (p)
    (if (rat? p) (f p) 'error)))
(define get-numer (check-rat cadr))
(define get-denom (check-rat cddr))

; примитивна капсулация на базовите операции
(define (make-rat n d)
  (lambda (prop)
    (case prop
      ('get-numer n)
      ('get-denom d)
      ('print (cons n d)))))

; нормализация при капсулация
(define (make-rat n d)
  (let* ((g (gcd n d))
         (numer (quotient n g))
         (denom (quotient d g)))
   (lambda (prop)
    (case prop
      ('get-numer numer)
      ('get-denom denom)
      ('print (cons numer denom))))))

; добавяне на аргументи към операциите
(define (make-rat n d)
  (let* ((g (gcd n d))
         (numer (quotient n g))
         (denom (quotient d g)))
   (lambda (prop . params)
     (case prop
       ('get-numer numer)
       ('get-denom denom)
       ('print (cons numer denom))
       ('* (let ((r (car params)))
            (make-rat (* numer (r 'get-numer))
                      (* denom (r 'get-denom)))))))))

; извикване на собствени операции
(define (make-rat n d)
  (let* ((g (gcd n d))
         (numer (quotient n g))
         (denom (quotient d g)))
   (define (self prop . params)
     (case prop
       ('get-numer numer)
       ('get-denom denom)
       ('print (cons numer denom))
       ('* (let ((r (car params)))
            (make-rat (* (self 'get-numer) (r 'get-numer))
                      (* (self 'get-denom) (r 'get-denom)))))))
   self))
