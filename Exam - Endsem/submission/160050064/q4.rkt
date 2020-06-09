#lang racket

(struct pbnode (info vec) #:transparent #:mutable)

(define (inserter pb name number)
  (let [(l (pbnode-info pb))]
      (define (mem-l ele l)
        (cond [(null? l) #f]
              [(equal? ele (caar l)) #t]
              [else (mem-l ele (cdr l))]))
    (define (compare<? p1 p2)
      (string<? (car p1) (car p2)))
    (if (mem-l name l) "Name already exists"
        (sort (cons (cons name number) l) compare<?))))
        
(define (insert-help pb name number l n)
  (if (equal? l 0) (inserter pb name number)
      (let* [(first (string-ref name n))
           (count (string-length name))
           (refer (cond [(or (equal? first #\a)
                            (equal? first #\b)
                            (equal? first #\c) 0)
                        (or (equal? first #\d)
                            (equal? first #\e)
                            (equal? first #\f) 1)
                        (or (equal? first #\g)
                            (equal? first #\h)
                            (equal? first #\i) 2)
                        (or (equal? first #\j)
                            (equal? first #\k)
                            (equal? first #\l) 3)
                        (or (equal? first #\m)
                            (equal? first #\n)
                            (equal? first #\o) 4)
                        (or (equal? first #\p)
                            (equal? first #\q)
                            (equal? first #\r)
                            (equal? first #\s) 5)
                        (or (equal? first #\t)
                            (equal? first #\u)
                            (equal? first #\v) 6)
                        (or (equal? first #\w)
                            (equal? first #\x)
                            (equal? first #\y)
                            (equal? first #\z) 7)]))
           (next-vec (pbnode-vec pb))
           (next (vector-ref next-vec refer))]
        (begin (when (equal? next #f) (vector-set! next-vec refer (pbnode '() (make-vector 8 #f))))
               (insert-help (vector-ref next-vec refer) name number (- l 1) (+ n 1))))))
        

(define (insert pb name number)
  (insert-help pb name number (string-length name) 0))
