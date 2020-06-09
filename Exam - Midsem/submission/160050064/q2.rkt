#lang racket

(provide subsets)

(define (subsets l)
  (if (null? l) '(())
      (let* [(prev (subsets (cdr l)))]
        (append prev (map (lambda (t) (cons (car l) t)) prev)))))
