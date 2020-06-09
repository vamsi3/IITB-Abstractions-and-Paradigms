#lang racket

(provide lcs)

(define (len l1 l2)
  (cond [(or (null? l1) (null? l2)) 0]
        [(= (car l1) (car l2)) (+ 1 (len (cdr l1) (cdr l2)))]
        [else (max (len (cdr l1) l2) (len (cdr l2) l1))]))

(define (lcs l1 l2)
  (cond [(or (null? l1) (null? l2)) '()]
        [(= (car l1) (car l2)) (cons (car l1) (lcs (cdr l1) (cdr l2)))]
        [(> (len (cdr l1) l2) (len (cdr l2) l1)) (lcs (cdr l1) l2)]
        [else (lcs (cdr l2) l1)]))
