#lang racket

(provide f)

(define f
  (let* [(s 0) (h 1)]
    (λ (x) (when (= h x) (set! s (- 1/2 s)))
      (set! h x) s)))