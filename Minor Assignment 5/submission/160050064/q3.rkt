#lang racket

(provide to-decimal)

(define (to-decimal char-l)
  (define ans 0)
  (define (to-decimal-help char-l)
    (if (null? char-l) ans
        (let* [(x (car char-l)) (y (cdr char-l))
               (temp 0) (count 0)]
          (define (decimal_part char-l)
            (define (repeat-10 n ans)
              (if (= n 0) ans
                  (repeat-10 (- n 1) (/ ans 10))))
            (if (null? char-l) (* temp (repeat-10 count 1))
                (let* [(x (car char-l))
                       (y (cdr char-l))]
                  (begin (set! temp (+ (* 10 temp) (- (char->integer x) 48)))
                         (set! count (+ count 1))
                         (decimal_part y)))))
          (if (equal? x #\.) (exact->inexact (+ ans (decimal_part y)))
              (begin (set! ans (+ (* 10 ans) (- (char->integer x) 48)))
                     (to-decimal-help y))))))
  (to-decimal-help char-l))

;; In this case, the *main* state variable is 'ans' which updates its value
;  for every iteration and finally gives the answer.