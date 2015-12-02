(define a 2)
a
(define a 5)
a

(define (make-sum! a)
  (lambda (inc)
    (set! a (+ a inc))
    a))

(define mysum (make-sum! 5))
(mysum 10)
(mysum 10)

;; не работи!
;;
;; (define (make-sum a)
;;   (lambda (inc)
;;     (define a (+ a inc))
;;     sum))
;;


(define (make-sum a)
  (lambda (inc)
    (let ((b (+ a inc)))
      (define a b)
      a)))

(define mysum (make-sum 5))
(mysum 10)
(mysum 10)

(define p (cons (cons 1 2) (cons 3 4)))
p
(set-car! (car p) 9)
p
(set-car! p 7)
p
; (set! (car p) 9)
(set-cdr! p '(5 3))
p
(set-cdr! (cdr p) p)
