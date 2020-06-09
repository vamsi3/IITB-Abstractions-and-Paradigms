#lang racket

(provide filtered-accumulate)
(provide f0)
(provide f1)
(provide f2)
(provide f3)
(provide f4)

;Define all functions below
;Do not call the functions here

(define (filtered-accumulate op1 op2 nxt min max id)
  (cond [(> min max) id]
        [(op1 (op2 min) (filtered-accumulate op1 op2 nxt (nxt min) max id))]))

(define (f0 x y)
  (define (min n) (if (= 1 (remainder n 13)) n  
                      (+ 1 (* 13 (+ 1 (quotient n 13))))))
  (filtered-accumulate + (lambda (x) x) (lambda (x) (+ 13 x)) (min x) y 0))

(define (f1 x y)
  (define (min n) (if (odd? n) n
                      (+ 1 n)))
  (filtered-accumulate + (lambda (x) (* x x)) (lambda (x) (+ 2 x)) (min x) y 0))

(define (f2 x y)
  (define (min n) (if (= 0 (remainder n 3)) n
                      (* 3 (+ 1 (quotient n 3)))))
  (define (fac n)
    (define (iter product counter)
      (if (> counter n)
          product
          (iter (* counter product)
                (+ counter 1))))
    (iter 1 1))
  (filtered-accumulate * fac (lambda (x) (+ x 3)) (min x) y 1))

(define (f3 x y)
  (define (is-prime n)
    (define (primec n count)
      (cond ((= n 2) #t)
            ((even? n) #f)
            ((> count (sqrt n)) #t)
            ((= (remainder n count) 0) #f)
            (else (primec n (+ count 2)))))
    (primec n 3))
  (define (min n)
    (if (is-prime n) n
        (min (+ n 1))))
  (define (nxt n) (min (+ n 1)))
  (filtered-accumulate + (lambda (x) (* x x)) nxt (min x) y 0))

(define (f4 x)
  (define (nxt n)
    (if (= 1 (gcd x (+ 1 n))) (+ n 1)
        (nxt (+ 1 n))))
  (filtered-accumulate * (lambda (x) x) nxt 1 x 1))
