#lang racket

(provide sub sub-single)

;Define functions sub and sub-single below
;Do not call the function here

(define (sub x y)
  (cond ((= x 0) (* -1 y))
        ((and (< x 0) (< y 0)) (sub (* -1 y) (* -1 x)))
        ((= x (* -1 y)) (* 2 x))
        ((< y 0) (if (> x (* -1 y)) (sub (* x 2) (sub x (* -1 y)))
                     (sub (* -1 y) (* -1 x))))
        (else (let* [(qx10 (quotient x 10))
                     (qy10 (quotient y 10))
                     (rx10 (remainder x 10))
                     (ry10 (remainder y 10))]
                (if (< rx10 ry10) (convert (sub (- qx10 1) qy10) (sub-single rx10 ry10 (< rx10 ry10)))
                    (convert (sub qx10 qy10) (sub-single rx10 ry10 (< rx10 ry10))))))))

(define (convert x y)
  (+ (* x 10) y))

(define (sub-single x y c)
  (cond ((and (integer? x) (integer? y) (boolean? c) (>= x 0) (>= y 0) (<= x 9) (<= y 9)) (if c (+ 10 (- x y)) (- x y)))
        (else #f)))

