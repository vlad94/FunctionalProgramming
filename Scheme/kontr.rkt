#lang scheme
(define m '((1 4 3)
            (4 5 6)
            (7 4 9)))
(define (getColumns m)
  (if (null? (car m)) '()
      (cons (map car m) (getColumns (map cdr m)))))
(define (allIn lst1 m)
  (cond [(null? lst1) #t]
        [(member (car lst1) (car m)) (allIn (cdr lst1) (car m))]  
        [else (allIn lst1 (cdr m))]))
(define (findColumns m)
  (cond [(null? (car m)) 0]
        [(allIn (car (getColumns m)) (car m)) (+ 1 (findColumns (map cdr m)))]
        [else (allIn (car (getColumns m)) (cdr m))]))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (helper f g a b)
  (accumulate append '() a b (lambda (x) (if (= (f x) (g x)) (list x) '() )) (lambda (x) (+ x 1))))

(define (longestInterval lst)
  (cond [(null? lst) '()]))
        