#lang racket

(provide evaluate)
(provide mult-8)
(provide div-8)

(define (mult-8 a b) 
  (let ([product (* a b)])
    (if (and (<= 0 product) (<= product 255)) product
        (error "Out of range"))))

(define (div-8 a b) 
  (let ([quotient (/ a b)])
    (if (and (<= 0 quotient ) (<= quotient 255)) quotient 
        (error "Out of range"))))

(define (evaluate la lb)
  (define (evaluate-help la lb)
    (cond [(and (null? la) (null? lb)) 1]
          [(null? la) (mult-8 (div-8 1 (car lb)) (evaluate-help la (cdr lb)))]
          [(null? la) (mult-8 (car la) (evaluate-help (cdr la) lb))]
          [else (mult-8 (div-8 (car la) (car lb)) (evaluate-help (cdr la) (cdr lb)))]))
  (evaluate-help (sort la >) (sort lb >)))
