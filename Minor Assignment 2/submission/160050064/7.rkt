#lang racket

(provide fib-lightning)
(provide fib-tr)

;Define all functions below
;Do not call the functions here

;I have considered f(0)=0 and f(1)=1 to proceed with the solution so as to match with the test cases,
;assuming that the given question has a minor error.

(define (fib-tr n)
  (define (fib-helper x value earlier1)
    (cond [(= x 0) 0]
          [(= x 1) value]
          [(> x 1) (fib-helper (- x 1) (+ value earlier1) value)]
          [else value]))
  (fib-helper n 1 0))

(define (fib-lightning n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [(even? n) (let* [(q (quotient n 2))
                          (a (fib-lightning q))
                          (b (fib-lightning (- q 1)))]
                     (* a (+ a (* 2 b))))]
        [else (let* [(q (quotient n 2))
                     (a (fib-lightning (+ q 1)))
                     (b (fib-lightning q))]
                (+ (* a a) (* b b)))]))