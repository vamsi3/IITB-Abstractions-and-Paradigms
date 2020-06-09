#lang racket

(provide gc)

(define (gc n)
  (if (= n 1) '((0) (1))
      (let* [(gc-minus (gc (- n 1)))
             (gc-minus-reverse (reverse gc-minus))]
        (append (map (lambda (x) (cons 0 x)) gc-minus)
                (map (lambda (x) (cons 1 x)) gc-minus-reverse)))))