(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define t (make-tree 1 (make-tree 2 empty-tree empty-tree)
                       (make-tree 3 (make-tree 4 empty-tree empty-tree)
                                    (make-tree 5 empty-tree empty-tree))))

(define (depth t)
  (if (empty-tree? t) 0
      (+ 1 (max (depth (left-tree t)) (depth (right-tree t))))))

(define (member-tree x t)
  (cond ((empty-tree? t) #f)
        ((eq? (root-tree t) x) t)
        (else (or (member-tree x (left-tree t)) (member-tree x (right-tree t))))))
