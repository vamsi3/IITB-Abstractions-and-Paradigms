#lang racket

(provide sum-of-last-three)

(define (sum-of-last-three)
  (define (sum-help x y)
    (let ((z (read)))
      (displayln (+ x y z))
      (sum-help y z)))
  (sum-help 0 0))

;; The state variable is 'z' which indirectly represents the
;  sum of the last-three numbers until a particular iteration.