#lang racket

(provide behave-like-c)

(define (behave-like-c)
  (define x 1)
  (define y 1)
  (define (helper x y k)
    (cond [(not (< k 10)) (+ x y)]
          [else (helper (+ k x (* 2 y)) (* 2 y) (+ k 1))]))
  (helper x y 0))