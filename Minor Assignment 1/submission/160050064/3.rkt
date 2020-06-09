#lang racket

(provide ak-mult)

;Define function ak-mult below
;Do not call the function here

(define (ak-mult x y)
  (define (prod x y c)
      (if (= x 0) c
          (cond ((even? x) (prod (quotient x 2) (* y 2) c))
                (#t (prod (quotient x 2) (* y 2) (+ c y))))))
  (prod x y 0))
