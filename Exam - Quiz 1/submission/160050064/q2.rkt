#lang racket

(provide connect)

(define (len-max l1 l2)
  (cond [(or (null? l1) (null? l2)) 0]
        [(= (car l1) (car l2)) (+ 1 (len-max (cdr l1) (cdr l2)))]
        [else (max (len-max (cdr l2) l1) (len-max (cdr l1) l2))]))

(define (connect l1 l2)
  (cond [(or (null? l1) (null? l2)) '()]
        [(= (car l1) (car l2)) (cons (car l1) (connect (cdr l1) (cdr l2)))]
        [(> (len-max (cdr l1) l2) (len-max (cdr l2) l1)) (connect (cdr l1) l2)]
        [else (connect l1 (cdr l2))]))