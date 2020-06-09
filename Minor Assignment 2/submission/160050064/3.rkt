#lang racket

(provide simpson)

;Define function simpson below
;Do not call the function here

(define (simpson f a b n)
  (define h (/ (- b a) (* 3 n)))
  (define (simp-helper f a b n)
    (cond [(= n 0) (f a)]
          [(even? n) (+ (* 2 (f (+ a (* 3 h n)))) (simp-helper f a b (- n 1)))]
          [(odd? n) (+ (* 4 (f (+ a (* 3 h n)))) (simp-helper f a b (- n 1)))]))
  (* h (- (simp-helper f a b n) (f b))))
