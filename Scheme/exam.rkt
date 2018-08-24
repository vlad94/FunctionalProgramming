#lang racket
(define a '(5 (3 (1 () (2 () ()))
       (4 () ()))
    (6 () ())))

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))
(define empty-tree '())
(define (make-tree root left right) (list root left right))      ; не искаме просто (define make-tree list) - защо?
(define (make-leaf root) (make-tree root empty-tree empty-tree)) ; за удобство
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define (tree-list t)
  (if (empty-tree? t) '()
      (append (tree-list (left-tree t))
              (list (root-tree t))
              (tree-list (right-tree t)))))

(define (intervalTree t)
  (if (empty-tree? t) '()
      (make-tree (cons (apply min (tree-list t)) (apply max (tree-list t))) (intervalTree (left-tree t)) (intervalTree (right-tree t)))))




  