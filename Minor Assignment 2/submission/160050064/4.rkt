#lang racket

(provide simplify)
(provide add)
(provide multiply)
(provide divide)

;Define all functions below
;Do not call the functions here

(define (gcd x y)
  (if (= y 0) x
      (gcd y (modulo x y))))

(define (simplify r)
  (cons (quotient (car r) (gcd (car r) (cdr r))) (quotient (cdr r) (gcd (car r) (cdr r)))))

(define (add r1 r2)
  (simplify (cons (+ (* (car r1) (cdr r2)) (* (cdr r1) (car r2))) (* (cdr r1) (cdr r2)))))

(define (multiply r1 r2)
  (simplify (cons (* (car r1) (car r2)) (* (cdr r1) (cdr r2)))))

(define (divide r1 r2)
  (simplify (cons (* (car r1) (cdr r2)) (* (car r2) (cdr r1)))))
