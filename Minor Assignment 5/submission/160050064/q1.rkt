#lang racket

(provide while)

(define-syntax while
  (syntax-rules ()
    [(while condition statements ...)
     (begin (define (iter)
              (when condition (begin statements ... (iter))))
            (iter))]))
