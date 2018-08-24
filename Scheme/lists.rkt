#lang scheme
(define (foldr* op nv lst)
  (if (null? lst) nv
      (op (car lst) (foldr* op nv (cdr lst)))))

(define (foldl* op nv lst)
  (if (null? lst) nv
      (foldl* op (op nv (car lst)) (cdr lst))))

(define (my-length l)
  (cond [(null? l) 0]
        [else (+ 1 (my-length (cdr l)))]))

(define (my-reverse l)
  (cond [(null? l) '()]
        [else (append (my-reverse (cdr l)) (list (car l)))]))

(define (nth n lst)
  (cond [(null? lst) #f]
        [(or (< n 0) (> n (length lst)) #f)]
        [(if (= n 0) (car lst)
            (nth (- n 1) (cdr lst)))]))

(define (range from to)
  (if (> from to) '()
      (append (list from) (range (+ from 1) to))))

(define (digit-list n)
  (if (< n 10) (list n)
      (append (digit-list (quotient n 10)) (list (remainder n 10)))))

(define (take n lst)
  (if (or (= n 0) (null? lst)) '()
      (append (list (car lst)) (take (- n 1) (cdr lst)))))
;neshto ne bachkat ako n e po-golqmo ot duljinata
(define (drop n lst)
  (if (or (= n 0) (null? lst)) lst
      (drop (- n 1) (cdr lst))))

(define (chunk n lst)
  (if (null? lst) '()
      (cons (take n lst) (chunk n (drop n lst)))))

(define (all p? lst)
  (if (null? lst) #t
      (and (p? (car lst)) (all p? (cdr lst)))))

(define (any p? lst)
  (if (null? lst) #f
      (or (p? (car lst)) (any p? (cdr lst)))))

(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2)) '()
      (append (list(cons (car lst1) (car lst2))) (zip (cdr lst1) (cdr lst2)))))

(define (remove-first val lst)
  (cond ((null? lst) '())
        ((= val (car lst)) (cdr lst))
        (else (append (list (car lst)) (remove-first val (cdr lst))))))

(define (remove-all val lst)
  (cond ((null? lst) '())
        ((= val (car lst)) (append '() (remove-all val (cdr lst))))
        (else (append (list (car lst)) (remove-all val (cdr lst))))))

(define (sumlst lst)
  (if (null? lst) 0
      (+ (car lst) (sumlst (cdr lst)))))
(define (sum-of-sums lst)
  (if (null? lst) '()
      (append (list(sumlst (car lst))) (sum-of-sums (cdr lst)))))

(define (extract-ints lst)
  (cond ((null? lst) '())
        ((number? (car lst)) (append (list(car lst)) (extract-ints (cdr lst))))
        (else (extract-ints (cdr lst)))))

(define (insert val lst)
  (cond ((or (null? lst) (< val (car lst))) (cons val lst))
        (else (cons (car lst) (insert val (cdr lst))))))

(define (insertion-sort lst)
  (if (null? lst) '()
      (insert (car lst) (insertion-sort (cdr lst)))))

  
