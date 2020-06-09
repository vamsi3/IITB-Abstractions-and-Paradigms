#lang racket

(provide goldbach)

;Define function goldbach below
;Do not call the function here

(define (modexp x y n)
  (cond ((= x 0) 1)
        ((= y 1) (modulo x n))
        ((even? y) (modulo (expt (modexp x (quotient y 2) n) 2) n))
        (#t (modulo (* (expt (modexp x (quotient y 2) n) 2) x) n))))

(define (is-prime n)
  (define (primec n count)
    (cond ((= n 1) #f)
          ((= n 2) #t)
          ((even? n) #f)
          ((> count (sqrt n)) #t)
          ((= (remainder n count) 0) #f)
          (else (primec n (+ count 2)))))
  (primec n 3))

(define (goldbach m)
  (define (first m x)
    (cond ((= m 2) #f)
          ((= m 4) (cons 2 2))
          ((odd? m) #f)
          ((and (is-prime x) (is-prime (- m x))) (cons x (- m x)))
          (else (first m (+ x 2)))))
  (first m 3))
