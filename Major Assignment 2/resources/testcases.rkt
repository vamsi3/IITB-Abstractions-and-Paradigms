#lang racket
(require "declarations.rkt")
(provide testList1 testList2)
(define testList1 
          (list
           (particle 5000.0 (vec 300 500) (vec 0.0 0.0))
           (particle 40.0 (vec 100 500) (vec 0.0 0.0))  
           (particle 200.0 (vec 500 500) (vec 0.0 0.0))))

(define testList2 
          (list
            (particle 40.0 (vec 300 600) (vec 30.0 0.0))
            (particle 5000.0 (vec 300 500) (vec 0.0 0.0))
            (particle 10.0 (vec  300 300) (vec -20 0.0))
            (particle 40.0 (vec 300 250.0) (vec -20 0.0))
            (particle 50.0 (vec 100 500) (vec 0 -20))
            (particle 10.0 (vec 400 500) (vec 0.0 15))
            (particle 50.0 (vec 300 650) (vec 25 0.0))
            (particle 10.0 (vec 300 100) (vec 15 0))
            (particle 7.0 (vec 200 500) (vec 0.0 -20))
            (particle 5.0 (vec 300 90) (vec 5 0.0))))
