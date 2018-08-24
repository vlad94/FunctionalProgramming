#lang scheme
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (fact-accum n)
  (accumulate * 1 1 n (lambda (x) x) (lambda (x) (+ x 1))))

(define (expt-accum x n)
  (accumulate * 1 1 n (lambda (i) x) (lambda (x) (+ x 1))))

(define (count-divisors n a b)
  (accumulate + 0 a b (lambda (x) (if (= (remainder n x) 0) 1 0)) (lambda (x) (+ x 1))))

(define (powers-sum x n)
  (accumulate + 0 1 n (lambda (i) (* (expt x i) i)) (lambda (x) (+ x 1))))

(define (prime-accum n)
  (define (and2 x y) (and x y))
  (accumulate and2 #t 2 (- n 1) (lambda (i) (if (= (remainder n i) 0) #f #t)) (lambda (x) (+ x 1))))

(define (!! n)
  (if (= (remainder n 2) 0) (accumulate * 1 2 n (lambda (x) x) (lambda (x) (+ x 2)))
                            (accumulate * 1 1 n (lambda (x) x) (lambda (x) (+ x 2)))))

(define (sum a b)
  (accumulate + 0 a b (lambda (x) x) (lambda (x) (+ x 1))))

(define (sum-iter a b term next)
  (define (helper res curr)
    (if (> curr b) res
        (helper (+ res (term curr)) (next curr))))
  (helper 0 a))

(define (derivative f)
  (define h 0.0000001)
  (lambda (x) (/ (- (f (+ x h)) (f x)) h)))

(define (flip f)
  (lambda (x y) (f y x)))
