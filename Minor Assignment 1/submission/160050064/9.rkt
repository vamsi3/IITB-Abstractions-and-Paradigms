#lang racket

(provide is-prime)

;Define function is-prime below
;Do not call the function here

(define (modexp x y n)
  (cond ((= x 0) 1)
        ((= y 1) (modulo x n))
        ((even? y) (modulo (expt (modexp x (quotient y 2) n) 2) n))
        (#t (modulo (* (expt (modexp x (quotient y 2) n) 2) x) n))))

(define (is-prime n)
  (cond ((= n 1) #f)
        ((or (= n 2) (= n 3)) #t)
        ((or (= (remainder n 6) 1) (= (remainder n 6) 5)) (define (prime n count)
                                                            (cond ((> count (quotient n 3)) #t)
                                                                  ((= (modexp (random n) (- n 1) n) 1) (prime n (+ count 1)))
                                                                  (else #f)))
                                                          (prime n 1))
        (else #f)))
