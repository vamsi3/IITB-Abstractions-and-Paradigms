#lang racket

(provide estimate-integral)

;Define function estimate-integral below
;Do not call the function here

(define (scaled-random x1 x2)
  (+ x1 (* (random)
           (- x2 x1))))

(define (estimate-integral p x1 x2 y1 y2 n)
  (define (helper k count)
    (cond [(= k 0) (abs (* (/ count n) (- x1 x2) (- y1 y2)))]
          [(p (scaled-random x1 x2) (scaled-random y1 y2)) (helper (- k 1) (+ count 1))]
          [else (helper (- k 1) count)]))
  (helper n 0))
