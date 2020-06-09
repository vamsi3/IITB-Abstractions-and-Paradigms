#lang racket

(define (rev-words str)
  (define (rev-words-help str ans)
    (if (equal? str "") ans
        (let [(first (string-ref str 0))]
          (cond [(equal? first #\space) (append ans (list #\space) (rev-words-help (substring str 1) '()))]
                [(equal? first #\newline) (append ans (list #\newline) (rev-words-help (substring str 1) '()))]
                [else (rev-words-help (substring str 1) (cons first ans))]))))
  (list->string (rev-words-help str '())))