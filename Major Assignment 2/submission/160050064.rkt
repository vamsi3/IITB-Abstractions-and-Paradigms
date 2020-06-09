#lang racket

(require "declarations.rkt")
(provide buildTree)
(provide calcForces)
(provide moveparticles)

;----------------------------- buildTree starts -------------------------------------

(define (buildTree initialArea particles)
  (cond [(null? particles) '()]
        [(null? (cdr particles)) (let* [(p (car particles))] (gnode (particle-mass p)
                                                                    (particle-posn p)
                                                                    '()))]
        [else
         (let* [(x1 (bbox-llx initialArea))
                (x3 (bbox-rux initialArea))
                (x2 (/ (+ x1 x3) 2))
                (y1 (bbox-lly initialArea))
                (y3 (bbox-ruy initialArea))
                (y2 (/ (+ y1 y3) 2))]
           (define (list-filter list-all ans)
             (if (null? list-all) ans
                 (let* [(carl (car list-all))
                        (cdrl (cdr list-all))]
                   (cond [(null? carl) (list-filter cdrl ans)]
                         [else (list-filter cdrl (cons carl ans))]))))
           (define (buildTree-help particles-help)
             (if (null? particles-help) (gnode mass-help
                                               (vec (/ x-help mass-help) (/ y-help mass-help))
                                               (list-filter (list (buildTree (bbox x2 y1 x3 y2) l4)
                                                                  (buildTree (bbox x1 y1 x2 y2) l3)
                                                                  (buildTree (bbox x2 y2 x3 y3) l2)
                                                                  (buildTree (bbox x1 y2 x2 y3) l1))
                                                            '()))
                 (let* [(p (car particles-help))
                        (mass_p (particle-mass p))
                        (posn_p (particle-posn p))
                        (x_p (vec-x posn_p))
                        (y_p (vec-y posn_p))]
                   (set! particles-help (cdr particles-help))
                   (set! mass-help (+ mass_p mass-help))
                   (set! x-help (+ (* mass_p x_p) x-help))
                   (set! y-help (+ (* mass_p y_p) y-help))
                   (cond [(< y_p y2) (cond [(< x_p x2) (set! l3 (cons p l3))]
                                           [else (set! l4 (cons p l4))])]
                         [else (cond [(< x_p x2) (set! l1 (cons p l1))]
                                     [else  (set! l2 (cons p l2))])])
                   (buildTree-help particles-help))))
           (define mass-help 0)
           (define x-help 0)
           (define y-help 0)
           (define l1 '()) (define l2 '()) (define l3 '()) (define l4 '())
           (buildTree-help particles))]))

;----------------------------- calcForces starts -------------------------------------

(define (calcForces initialArea tree particles)
  (define (sq x) (* x x))
  (let* [(s (- (bbox-rux initialArea) (bbox-llx initialArea)))]
    (define (force-find p)
      (define forcex-help 0)
      (define forcey-help 0)
      (let* [(mass_p (particle-mass p))
             (posn_p (particle-posn p))
             (x_p (vec-x posn_p))
             (y_p (vec-y posn_p))]
        (define (calcForces-help tree-help s)
          (let* [(list_c (gnode-subtrees tree-help))
                 (posn_c (gnode-posn tree-help))
                 (x_c (vec-x posn_c))
                 (y_c (vec-y posn_c))
                 (a (- x_c x_p))
                 (b (- y_c y_p))
                 (d-squared (+ (sq a) (sq b)))
                 (d (sqrt d-squared))]
            (cond [(or (null? list_c)
                       (> (/ d s) theta)) (if (= d-squared 0) 0
                                              (let* [(mass_c (gnode-mass tree-help))
                                                     (force-coeff (/ (* g mass_p mass_c) (* d-squared d)))]
                                                (set! forcex-help (+ forcex-help (* force-coeff a)))
                                                (set! forcey-help (+ forcey-help (* force-coeff b)))))]
                  [else (begin (define (loop list_c)
                                 (let* [(x (car list_c))]
                                   (if (null? (cdr list_c)) (calcForces-help x (/ s 2))
                                       (begin (calcForces-help x (/ s 2))
                                              (loop (cdr list_c))))))
                               (loop list_c))])))
        (calcForces-help tree s)
        (vec forcex-help forcey-help)))
    (map force-find particles)))

;----------------------------- moveparticles starts -------------------------------------

(define (moveparticles particles forces)
  (define (move-particle p)
    (let* [(force (car forces))
           (fx (vec-x force))
           (fy (vec-y force))
           (mass_p (particle-mass p))
           (mass_p2 (* 2 mass_p))
           (posn_p (particle-posn p))
           (x (vec-x posn_p))
           (y (vec-y posn_p))
           (vel (particle-velocity p))
           (vx (vec-x vel))
           (vy (vec-y vel))
           (ax-half (/ fx mass_p2))
           (ay-half (/ fy mass_p2))
           (sq-time (* timeslice timeslice))]
      (set! forces (cdr forces))
      (set! x (+ x (* vx timeslice) (* ax-half sq-time)))
      (set! y (+ y (* vy timeslice) (* ay-half sq-time)))
      (set! vx (+ vx (* 2 ax-half timeslice)))
      (set! vy (+ vy (* 2 ay-half timeslice)))
      (particle mass_p (vec x y) (vec vx vy))))
  (map move-particle particles))

;------------------------------------ CODE ENDS --------------------------------------