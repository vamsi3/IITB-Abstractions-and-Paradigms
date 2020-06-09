#lang racket
(provide choose memo-choose)


(define choose
  (lambda (n k) 
    (if (or (zero? k) (= n k))
        1
        (+ (choose (- n 1) (- k 1))
           (choose (- n 1) k)))))

(define (memo-choose n k)
  (define memo (make-2d-vector (+ n 1) (+ k 1)))
  (define (memo-choose-help n k)
    (cond [(or (zero? k) (= n k)) 1]
          [else (let ((recall (2d-vector-ref memo n k)))
                  (cond [(not (equal? #f recall)) recall]
                        [else (let ((ans (+ (memo-choose-help (- n 1) (- k 1)) (memo-choose-help (- n 1) k))))
                                (2d-vector-set! memo n k ans)
                                ans)]))]))
  (memo-choose-help n k)
)


(define (make-2d-vector r c)
  (build-vector r 
    (lambda (x) (make-vector c #f))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val)
      (vector-set! vec r v))))

