#lang racket

(provide fewest-moves)

(define (fewest-moves l)
  ;; 'check?' checks for right input.
  (define (check? l) (equal? (remove* (list 0 1) l) '()))
  (cond [(null? l) 0]
        [(= (car l) 0) (if (<= (length l) 4) (if (check? l) 1 (error "Not a correct input."))
                           (+ 1 (min (fewest-moves (cdr l)) (fewest-moves (cddddr l)))))]
        [(= (car l) 1) (if (<= (length l) 2) (if (check? l) 1 (error "Not a correct input."))
                           (+ 1 (min (fewest-moves (cdr l)) (fewest-moves (cddr l)))))]
        [else (error "Not a correct input.")]))