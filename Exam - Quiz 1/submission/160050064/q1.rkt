#lang racket

(provide calc-pieces)

(define (extended-euclid x y)
  (if (= y 0) (cons 1 0)
  (let* [(p (extended-euclid y (modulo x y)))
         (a (car p))
         (b (cdr p))]
    (cons b (- a (* b (quotient x y)))))))

(define (calc-pieces a b w)
  (cond [(not (= (remainder w (gcd a b)) 0)) '()]
        [else (let* [(q (quotient w (gcd a b)))
                     (x (car (extended-euclid a b)))
                     (y (cdr (extended-euclid a b)))]
                (cond [(< x 0) (list (list (list (* q y) b)) (list (list (abs (* q x)) a) w))]
                      [else (list (list (list (* q x) a)) (list (list (abs (* q y)) b) w))]))]))