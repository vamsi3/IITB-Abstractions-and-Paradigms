 #lang racket
(require "declarations.rkt")
(require "drawing-routine.rkt")
(require "testcases.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; singlestep :: (Particle) -> (Particle)

(define (singlestep particles)
  (let* ([initialArea (bounding-box particles)]
         [tree (buildTree initialArea particles)] 
         [forces (calcForces initialArea tree particles)])
    (moveparticles particles forces)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;main will send the current vectors for printing
;calculate new vectors and recurse

;;main :: (Particle) -> Action
(define (main ps)
  (define (main-helper ps i)
    (cond [(> i iter) (display "Done")]
          [else (let*
                    ([ps-next (singlestep ps)])
                  (if (= (remainder iter drawtime) 0)
                      (begin 
                        (draw-particles ps)
                        (main-helper ps-next (+ i 1)))
                      (main-helper ps-next (+ i 1))))]))
  (main-helper ps 0))

(main testList2)