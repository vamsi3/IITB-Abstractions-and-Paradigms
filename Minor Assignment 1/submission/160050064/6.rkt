#lang racket

;Define function modexp below
;Do not call the function here

(define (modexp x y n)
  (define (modexp1 x y n)
    (cond ((= y 0) (if (= x 0) 0 1))
          ((= y 1) (modulo x n))
          ((even? y) (modulo (expt (modexp1 x (quotient y 2) n) 2) n))
          (#t (modulo (* (modulo (expt (modexp1 x (quotient y 2) n) 2) n) x) n))))
  (cond ((>= x 0) (modexp1 x y n))
        ((even? y) (modexp1 (* -1 x) y n))
        (else (modulo (* -1 (modexp1 (* -1 x) y n)) n))))
