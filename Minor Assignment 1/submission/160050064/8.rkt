#lang racket

;Define function inverse below
;Do not call the function here

(define (inverse e n)  
  (define (eea x y)
    (cond ((= y 0) (cons 1 0))
          (#t (let* [(a (car (eea y (modulo x y))))
                     (b (cdr (eea y (modulo x y))))]
                (cons b (- a (* (quotient x y) b)))))))
  (let* [(a (car (eea (abs e) n)))
         (b (cdr (eea (abs e) n)))]
    (cond ((not (= (+ (* a (abs e)) (* b n)) 1)) -1)
          ((< e 0) (modulo (* -1 (modulo a n)) n))
          (else (modulo a n)))))
