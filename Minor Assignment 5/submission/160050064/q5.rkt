#lang racket

(provide make-account make-joint)

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch n m)
    (cond [(not (eq? n password)) (error "Incorrect password")]
          [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else (error "Unknown request -- "
                       m)]))
  dispatch)

(define (make-joint acc1 password1 password2)
  (begin
    (define check ((acc1 password1 'deposit) 0))
    (define (dispatch n m)
      (cond [(not (eq? n password2)) (error "Incorrect password")]
            [else (acc1 password1 m)]))
    dispatch))
