#lang scheme
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))
(define (meetTwice? f g a b)
  (if (< (accumulate + 0 a b (lambda (x) (if (= (f x) (g x)) 1 0)) (lambda (x) (+ x 1))) 2) #f #t))

(define (duplicate x lst)
  (cond [(null? lst) 0]
        [(= x (car lst)) (+ 1 (duplicate x (cdr lst)))]
        [else (duplicate x (cdr lst))]))
(define (duplicate? x lst)
  (if (<= 2 (duplicate x lst)) #t #f))

(define (remove-all x lst)
  (filter (lambda (y) (not (equal? x y))) lst))

(define (find-duplicates lst)
  (cond [(null? lst) '()]
        [(duplicate? (car lst) lst) (cons (car lst)
                                             (find-duplicates (remove-all (car lst) (cdr lst))))]
        [else (find-duplicates (cdr lst))]))

(define (max-duplicate ll)
  (let [(temp (apply append (map find-duplicates ll)))]
    (if (null? temp) #f (apply max temp))))

(define (checkrem lst k)
  (if (null? lst) #f
      (or (= (remainder (car lst) k) 0) (checkrem (cdr lst) k))))
(define (checkmatrix? m k)
  (if (or (null? m) (null? (car m))) #t
      (and (checkrem (car m) k) (checkmatrix? (cdr m) k))))


(define (or2 x y)
  (if x #t y))
(define (mixed? f g a b)
  (and (accumulate or2 #f a b (lambda (x) (if (> f(x) g(x)) #t #f)) (lambda (x) (+ 1 x))) 
       (accumulate or2 #f a b (lambda (x) (if (< f(x) g(x)) #t #f)) (lambda (x) (+ 1 x)))))

(define (unique x lst)
  (cond [(null? lst) 0]
        [(= x (car lst)) (+ 1 (unique x (cdr lst)))]
        [else (unique x (cdr lst))]))
(define (unique? x lst)
  (if (< 1 (unique x lst)) #f #t))
(define (finduniques lst)
  (cond [(null? lst) '()]
        [(unique? (car lst) lst) (cons (car lst) (finduniques (cdr lst)))]
        [else (finduniques (remove-all (car lst) (cdr lst)))]))
(define (maxUnique ll)
  (apply max (apply append (map finduniques ll))))

(define (occurrences lst1 lst2)
  (define (countocc x lst)
    (cond [(null? lst) 0]
          [(= x (car lst)) (+ 1 (countocc x (cdr lst)))]
          [else (countocc x (cdr lst))]))
  (if (null? lst1) '()
      (cons (countocc (car lst1) lst2)
            (occurrences (cdr lst1) lst2))))