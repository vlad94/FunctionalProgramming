#lang scheme
(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

(define (countdigit n)
  (if (< n 10) 1
      (+ 1 (countdigit (/ n 10)))))

(define (reverse-int n)
  (if (< n 10) n
      (+ (* (remainder n 10) (expt 10 (- (countdigit n) 1))) (reverse-int (quotient n 10)))))

(define (palindrome? n)
  (if (= n (reverse-int n)) #t #f))

;zashto drenski nqma if

(define (divisors-sum n)
  (define (helper sum div)
    (cond [(< n div) sum]
          [(= (remainder n div) 0) (helper (+ sum div) (+ div 1))]
          [(helper sum (+ div 1))]))
  (helper 0 1))

(define (perfect? n)
  (if (= (- (divisors-sum n) n) n) #t #f))

(define (prime? n)
  (define (helper i)
    (cond [(< n 2) #t]
          [(= n i) #t]
          [(= (remainder n i) 0) #f]
          [else (helper (+ i 1))]))
  (helper 2))

(define (increasing? n)
  (cond [(< n 10) #t]
        [(> (remainder n 10) (remainder (quotient n 10) 10)) (increasing? (quotient n 10))]
        [else #f]))


