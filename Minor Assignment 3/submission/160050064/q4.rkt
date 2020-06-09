#lang racket

(provide rle)

(define (rle l)
  (foldr op '() l))

(define (op x y)
  (if (null? y) (cons (list x 1) '())
  (if (= (caar y) x) (next-same y)
      (next-different y x))))

(define (next-same l)
  (cons (list (caar l) (+ (cadar l) 1)) (cdr l)))

(define (next-different l n)
  (cons (list n 1) l))