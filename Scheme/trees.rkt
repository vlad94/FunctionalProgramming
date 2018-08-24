#lang scheme
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

(define (tree-height t)
  (if (empty-tree? t)
      -1
      (+ 1 (max (tree-height (left-tree t))
                (tree-height (right-tree t))))))

(define a '(5 (3 (1 () (2 () ()))
       (4 () ()))
    (6 () ())))

(define (tree-sum t)
  (if (empty-tree? t) 0
      (+ (root-tree t) (tree-sum (left-tree t)) (tree-sum (right-tree t)))))

(define (tree-max t)
  (if (empty-tree? t) -inf.0
      (max (root-tree t) (tree-max (left-tree t)) (tree-max (right-tree t)))))

(define (tree-level k t)
  (cond [(empty-tree? t) '()]
        [(= k 0) (list (root-tree t))]
        [else (append (tree-level (- k 1) (left-tree t))
                      (tree-level (- k 1) (right-tree t)))]))

(define (all-levels t)
  (map (lambda (x) (tree-level x t)) (range 0 (+ (tree-height t) 1))))

(define (tree-map f t)
  (if (empty-tree? t) '()
      (make-tree (f (root-tree t)) (tree-map f (left-tree t)) (tree-map f (right-tree t)))))

(define (tree-list t)
  (if (empty-tree? t) '()
      (append (tree-list (left-tree t))
              (list (root-tree t))
              (tree-list (right-tree t)))))

(define (bst-insert val t)
  (cond [(empty-tree? t) (make-leaf val)]
        [(< val (root-tree t)) (make-tree (root-tree t)
                                          (bst-insert val (left-tree t))
                                          (right-tree t))]
        [(make-tree (root-tree t)
                    (left-tree t)
                    (bst-insert val (right-tree t)))]))

(define (list->tree lst)
  (if (null? lst)
      empty-tree
      (bst-insert (car lst)
                  (list->tree (cdr lst)))))
(define (tree-sort lst) (tree-list (list->tree lst)))

(define (isleaf? n)
  (if (and (not (null? (car n)))
           (null? (cadr n))
           (null? (caddr n))) #t #f))

(define (bloom t)  
  (cond [(empty-tree? t) '()]
        [(isleaf? t) (make-tree (root-tree t) (make-leaf (root-tree t)) (make-leaf (root-tree t)))]
        [else (make-tree (root-tree t) (bloom (left-tree t)) (bloom (right-tree t)))]))

(define (intervalTree t)
  (if (empty-tree? t) '()
      (make-tree (cons (apply min (tree-list t)) (apply max (tree-list t))) (intervalTree (left-tree t)) (intervalTree (right-tree t)))))

