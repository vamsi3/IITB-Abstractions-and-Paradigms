#lang racket

(provide has-solution)

;Define function has-solution below
;Do not call the function here

(define (gcd x y)
  (if (= y 0) x
      (gcd y (modulo x y))))

(define (has-solution a b c)
  (cond ((and (= a 0) (= b 0)) (if (= c 0) #t #f))
        ((= (remainder c (gcd (abs a) (abs b))) 0) #t)
        (else #f)))
