#lang racket

(provide minchange)

;Define function minchange below
;Do not call the function here

(define (minchange n)
  (define denom (list 50 25 20 10 5 3 2 1))
  (define (greedy n l)
    (if (null? (cdr l)) (quotient n (first l))
        (+ (quotient n (first l)) (greedy (remainder n (first l)) (cdr l)))))
  (define (coin-help sum last-den case minx)
    (cond [(> case minx) +inf.0]
          [(= sum 0) case]
          [(and (= (last denom) (list-ref denom last-den)) (< sum (list-ref denom last-den))) +inf.0]
          [(and (= (last denom) (list-ref denom last-den)) (>= sum (list-ref denom last-den))) (coin-help (- sum (list-ref denom last-den)) last-den (+ case 1) minx)]
          [(< sum (list-ref denom last-den)) (coin-help sum (+ last-den 1) case minx)]
          [else (min (coin-help (- sum (list-ref denom last-den)) last-den (+ 1 case) minx) (coin-help sum (+ last-den 1) case minx))]))
  (coin-help n 0 0 (greedy n denom)))
