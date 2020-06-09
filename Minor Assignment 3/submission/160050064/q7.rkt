#lang racket

(provide check-a)
(provide check-b)
(provide check-c)

(define (check-a l)
  (define (check-equal? l)
    (if (equal? '(1 2 3 4 5 6 7 8 9) (sort l <)) #t #f))
  (define (op x y) (if (false? y) #f (check-equal? x)))
  (foldr op #t l))

(define (check-b l)
  (define (b-to-a n)
    (if (= n 9) '()
        (cons (append (map (lambda (t) (list-ref t n)) l)) (b-to-a (+ n 1)))))
  (check-a (b-to-a 0)))

(define (check-c l)
  (define l1 (list (take l 3) (take (drop l 3) 3) (drop l 6)))
  (check-a (append* (map (lambda (l) (list (append* (map (lambda (l) (take l 3)) l))
                                           (append* (map (lambda (l) (take (drop l 3) 3)) l))
                                           (append* (map (lambda (l) (drop l 6)) l))))
                         l1))))

;; I have used the predefined library functions 'sort' 'take' 'drop' 'list-ref' with reference from the professor.
; Still I would like to make the evaluation unambigious. So, I am giving out a commented-out my versions of these
; functions

;; filter function (used for sort function)

;(define (filter p l)
;  (cond [(null? l) '()]
;        [(p (car l)) (cons (car l) (filter p (cdr l)))]
;        [else (filter p (cdr l))]))

;; sort function

;(define (sort l op)
;  (if (null? l) '()
;      (append (sort (filter (lambda (x) (op x (car l))) l) op)
;              (list (car l))
;              (sort (filter (lambda (x) (not (or (= x (car l)) (op x (car l))))) l) op))))

;; take function

;(define (take l n)
;  (if (= n 0) '()
;      (cons (car l) (take (cdr l) (- n 1)))))

;; drop function

;(define (drop l n)
;  (if (= n 0) l
;      (drop (cdr l) (- n 1))))

;; list-ref function

;(define (list-ref l n)
;  (if (= n 0) (car l)
;      (list-ref (cdr l) (- n 1))))