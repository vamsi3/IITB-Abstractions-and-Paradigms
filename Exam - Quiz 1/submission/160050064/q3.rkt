#lang racket

(provide listify-function)
(provide list-+)
(provide list-*)

(define (listify-function l op id)
  (define (operator l id)
    (if (null? l) id
        (op (car l) (operator (cdr l) id))))
  (cond [(null? l) '()]
        [(null? (car l)) '()]
        [else (cons (operator (map (lambda (subl) (car subl)) l) id)
                    (listify-function (map (lambda (subl) (cdr subl)) l) op id))]))

(define (list-+ l)
  (listify-function l + 0))

(define (list-* l)
  (listify-function l * 1))