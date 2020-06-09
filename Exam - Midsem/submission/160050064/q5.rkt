#lang racket

(provide to-btree)
(provide bitwise-or)
(provide findvalue)

(struct bnode (ltree rtree) #:transparent)
(struct leaf (val) #:transparent)

(define (to-btree bm)
  (define (to-btree-help bm len)
    (cond [(null? (remove* (list 1) bm)) (leaf 1)]
          [(null? (remove* (list 0) bm)) (leaf 0)]
          [else (let ((a (/ len 2)))
                  (bnode (to-btree-help (take bm a) a) (to-btree-help (drop bm a) a)))]))
  (to-btree-help bm (length bm)))

(define (bitwise-or bt1 bt2)
    (cond [(and (equal? (bnode-ltree bt1) (leaf 1))
                (equal? (bnode-rtree bt2) (leaf 1))) (bnode (leaf 1) (leaf 1))]
          [(and (equal? (bnode-rtree bt1) (leaf 1))
                (equal? (bnode-rtree bt2) (leaf 1))) (bnode (leaf 1) (leaf 1))]
          [(or (equal? (bnode-ltree bt1) (leaf 1))
               (equal? (bnode-ltree bt2) (leaf 1))) (bnode (leaf 1) (bitwise-or (bnode-rtree bt1)
                                                                                (bnode-rtree bt2)))]
          [(or (equal? (bnode-rtree bt1) (leaf 1))
               (equal? (bnode-rtree bt2) (leaf 1))) (bnode (bitwise-or (bnode-ltree bt1)
                                                                       (bnode-ltree bt2))
                                                           (leaf 1))]
          [else (bnode (bitwise-or (bnode-ltree bt1)
                                   (bnode-ltree bt2))
                       (bitwise-or (bnode-rtree bt1)
                                   (bnode-rtree bt2)))]))

(define (findvalue index n bt)
  (cond [(leaf? bt) (leaf-val bt)]
        [(> index (/ n 2)) (findvalue (- index (/ n 2)) (/ n 2) (bnode-rtree bt))]
        [else (findvalue index (/ n 2) (bnode-ltree bt))]))