#lang racket

;Define function coeffs below
;Do not call the function here

(define (coeffs a b)
  (if (= b 0) (cons 1 0)
      (let* [(x (car (coeffs b (modulo a b))))
             (y (cdr (coeffs b (modulo a b))))]
        (cons y (- x (* (quotient a b) y))))))
