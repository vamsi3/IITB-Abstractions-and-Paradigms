#lang racket

(provide summands)

(define (summands n)
  (define (step t) (map (lambda (x) (cons (- n t) x)) (summands t)))
  (define (f t)
    (if (= t 0) (step 0) (append (step t) (f (- t 1)))))
  (if (= n 0) '(()) (f (- n 1))))