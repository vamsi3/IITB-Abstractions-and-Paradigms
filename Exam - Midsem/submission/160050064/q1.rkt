#lang racket

(provide cansurvive?)

(define (cansurvive? pos n)
  (cond [(< pos 0) (cansurvive? (+ n 1 pos) n)]
        [(= pos 0) #f]
        [(or (= n 2) (= n 1)) #t]
        [else (cansurvive? (- pos 3) (- n 1))]))
