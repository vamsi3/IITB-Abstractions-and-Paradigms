#lang racket

(provide cprod)

(define (cprod l)
  (define (step n)
    (map (lambda (t) (cons n t)) (cprod (cdr l))))
  (if (null? l) '(())
      (append* (map step (car l)))))