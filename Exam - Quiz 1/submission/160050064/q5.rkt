#lang racket

(provide flatten)

(struct node (val ltree rtree) #:transparent)
(struct nulltree () #:transparent)

(define (flatten t)
  (define (flatten-helper t l)
    (cond [(and (nulltree? (node-ltree t)) (nulltree? (node-rtree t))) (cons (node-val t) l)]
          [(nulltree? (node-ltree t)) (cons (node-val t) (flatten-helper (node-rtree t) l))]
          [(nulltree? (node-rtree t)) (flatten-helper (node-ltree t) (cons (node-val t) l))] 
          [else (flatten-helper (node-ltree t) (cons (node-val t) (flatten-helper (node-rtree t) l)))]))
  (flatten-helper t '()))