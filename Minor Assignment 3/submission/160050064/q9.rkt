#lang racket

(provide shuffle)

(define (shuffle xs1 xs2)
  (cond [(null? xs1) (list xs2)]
        [(null? xs2) (list xs1)]
        [else  (append (map (lambda (l) (cons (car xs1) l)) (shuffle (cdr xs1) xs2))
                       (map (lambda (l) (cons (car xs2) l)) (shuffle xs1 (cdr xs2))))]))

;; My function works as follows -

; Notice that the first element of the lists beloning to the list (shuffle xs1 xs2) is either among the
; first elements of lists xs1 and xs2 necessarily. Hence, we consider both these cases, call the shuffle
; function recursively for each case and then append them. During the recursive calls, if either of xs1
; or xs2 becomes null (the shorter list becomes null first), the other list is simply returned by nesting
; it in an empty list. So, the append does its work and the shuffle function is complete.