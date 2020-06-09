#lang racket

(define account%
  (class object%
    (init balance)
    (define bal balance)
    (init-field interest-rate)
    (init passwd)
    (define password passwd)
    (super-new)
    
    (define/public (withdraw amount pass)
      (if (not (equal? pass password)) (error "Wrong Password")
               (begin 
               (if (< bal amount) (error "Insufficient Balance")
                   (set! bal (- bal amount))))))
    
    (define/public (deposit amount)
      (set! bal (+ bal amount)))
    
    (define/public (show pass)
      (if (equal? pass password) bal
          (error "Wrong Password")))
    
    (define (pay-interest) 0)))

(define timer%
  (class object%
    (init-field this-timer)
    (super-new)))

(define credit-card%
  (class account%
    (super-new)
    (