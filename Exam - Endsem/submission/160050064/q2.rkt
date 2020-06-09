#lang racket

(define (voltage Is Vt I R)
  (define V 1)
  (define iter 500)
  (define (help n)
    (if (= n 0) V
        (let* [(k (* R Is (exp (/ V Vt))))]
          (set! V (- V (/ (+ (- k (* R (+ I Is))) V) (+ (/ k Vt) 1))))
          (help (- n 1)))))
  (help iter))