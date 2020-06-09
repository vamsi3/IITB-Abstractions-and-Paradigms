#lang racket

(provide decode)

;Define function decode below
;Do not call the function here

(define (modexp x y n)
  (cond ((= y 0) (if (= x 0) 0 1))
        ((= y 1) (modulo x n))
        ((even? y) (modulo (expt (modexp x (quotient y 2) n) 2) n))
        (#t (modulo (* (modulo (expt (modexp x (quotient y 2) n) 2) n) x) n))))

(define (inverse e n)  
  (define (eea x y)
    (cond ((= y 0) (cons 1 0))
          (#t (let* [(a (car (eea y (modulo x y))))
                     (b (cdr (eea y (modulo x y))))]
                (cons b (- a (* (quotient x y) b)))))))
  (let* [(a (car (eea (abs e) n)))
         (b (cdr (eea (abs e) n)))]
    (cond ((not (= (+ (* a (abs e)) (* b n)) 1)) -1)
          ((< e 0) (* -1 (modulo a n)))
          (else (modulo a n)))))

(define (decode p q e c)
  (modexp c (inverse e (* (- p 1) (- q 1))) (* p q)))
