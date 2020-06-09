#lang racket

(provide pascal)

(define (pascal n)
  (define (next l)
    (if (null? (cdr l)) (list 1)
        (cons (+ (first l) (second l)) (next (cdr l)))))
  (if (= n 0) (list (list 1))
      (let* [(p (pascal (- n 1)))]
        (append p (list (cons 1 (next (last p))))))))
