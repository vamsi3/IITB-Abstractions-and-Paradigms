#lang racket

;Define function div below
;Do not call the function here



(define (div x y)
  (define (divx x y)
    (cond ((= x 0) (cons 0 0))
          (#t (let* [(q (car (divx (quotient x 2) y)))
                     (r (cdr (divx (quotient x 2) y)))]
                (cond ((even? x) (cons (+ (* q 2) (quotient (* r 2) y)) (remainder (* r 2) y)))
                      (else (cons (+ (* q 2) (quotient (+ (* r 2) 1) y)) (remainder (+ (* r 2) 1) y))))))))
  (cond ((= y 0) #f)
        ((< x 0) (if (= (cdr (divx x y)) 0) (cons (* -1 (car (divx x y))) (modulo (* -1 (cdr (divx x y))) y))
                     (cons (* -1 (+ 1 (car (divx x y)))) (modulo (* -1 (cdr (divx x y))) y))))
        (else (divx x y))))