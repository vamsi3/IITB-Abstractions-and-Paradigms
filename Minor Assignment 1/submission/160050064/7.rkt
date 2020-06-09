#lang racket

(provide carmichael)

;Define function carmichael below
;Do not call the function here

(define (gcd x y)
  (if (= y 0) x
      (gcd y (modulo x y))))

(define (modexp x y n)
  (cond ((= x 0) 1)
        ((= y 1) (modulo x n))
        ((even? y) (modulo (expt (modexp x (quotient y 2) n) 2) n))
        (#t (modulo (* (expt (modexp x (quotient y 2) n) 2) x) n))))

(define (is-prime n)
  (define (primec n count)
    (cond ((= n 2) #t)
          ((even? n) #f)
          ((> count (sqrt n)) #t)
          ((= (remainder n count) 0) #f)
          (else (primec n (+ count 2)))))
  (primec n 3))

(define (car-check x iter)
  (if (is-prime x) #f
      (if (< iter x) (if (= (gcd iter x) 1) (if (= (modexp iter (- x 1) x) 1) (car-check x (+ iter 1))
                                            #f)
                     (car-check x (+ iter 1)))
      #t)))

(define (carmichael n)
  (define (mich n x count)
    (if (< count n) (if (car-check x 1) (mich n (+ x 2) (+ count 1))
                        (mich n (+ x 2) count))
        (- x 2)))
  (mich n 561 0))
