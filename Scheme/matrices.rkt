#lang scheme
(define (all p? l)
 (foldr (lambda (x y) (and x y)) #t l))

(define (matrix? m)
  (and (list? m)
       (not (null? (car m)))
       (all list? m)
       (all (lambda (row) (= (length row)
                             (length (car m)))) m)))

(define (get-columns m)
  (length (car m)))

(define (drop n lst)
  (if (or (= n 0) (null? lst)) lst
      (drop (- n 1) (cdr lst))))

(define (get-row i m) (list-ref m i))
(define (get-column i m)
  (map(lambda (row) (list-ref row i)) m))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))
(define (transpose m)
  (accumulate cons '() 0 (- (get-columns m) 1) (lambda (i) (get-column i m)) (lambda (x) (+ x 1))))

(define (sum-vectors v1 v2) (map + v1 v2))

;zadacki 6to uprajnenie
;
;
(define (begins-with? lst1 lst2)
  (cond [(and (null? lst1) (null? lst2)) #t]
        [(null? lst1) #t]
        [(null? lst2) #f]
        [(equal? (car lst1) (car lst2)) (begins-with? (cdr lst1) (cdr lst2))]
        [else #f]))
         
(define (sublist? lst1 lst2)
  (cond [(null? lst2) (null? lst1)]
        [(begins-with? lst1 lst2) #t]
        [else (sublist? lst1 (cdr lst2))]))

(define (remove-all x lst)
  (filter (lambda (y) (not (equal? x y))) lst))

(define (make-set lst)
  (if (null? lst) '()
      (cons (car lst) (make-set (remove-all (car lst) (cdr lst))))))

(define (count-occ x lst)
  (cond [(null? lst) 0]
        [(equal? x (car lst)) (+ 1 (count-occ x (cdr lst)))]
        [else (count-occ x (cdr lst))]))
(define (histogram lst)
  (if (null? lst) '()
      (map (lambda (x) (cons x (count-occ x lst))) (make-set lst))))

(define (all? p? lst)
  (cond [(null? lst) #t]
        [(not (p? (car lst))) #f]
        [else (all? p? (cdr lst))]))

(define (triangular? m)
  (define (allZero col) (all? (lambda (x) (= x 0)) col))
  (if (or (null? (car m)) (null? (cdr m))) ; за да обработва коректно и квадратни, и правоъгълни матрици
      #t      ; първата колона, без първия ред
      (and (allZero (cdr (map car m)))
           (triangular? (cdr (map cdr m))))))

(define (null-mat? m)
  (or (null? m) (null? (car m))))

(define (main-diag m)
  (if (or (null? m) (null? (car m))) '()
      (cons (car (car m)) (main-diag (cdr (map cdr m))))))

(define (2nd-diag m)
  (main-diag (map reverse m)))

(define (descartes lst1 lst2) ; map-ception
(apply append (map (lambda (x) (map (lambda (y) (cons x y)) lst2)) lst1)))
        
  


  
