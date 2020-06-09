#lang racket

(provide reverse-tr)

(define (reverse-tr l)
  (define (r-help id l)
    (if (null? l) id
        (r-help (cons (car l) id) (cdr l))))
  (r-help '() l))
