#lang racket
(provide (struct-out particle) (struct-out gnode)
         (struct-out bbox) (struct-out vec)
         theta iter g timeslice drawtime zipwith
	 singleton sum concat lc bounding-box)

; A particle is a struct with  a record of its mass, its location, and
; its velocity:

(struct particle (mass posn velocity) #:transparent)

;The struct representing the tree

(struct gnode (mass posn subtrees) #:transparent)

;Struct for the bounding box. Specifies the box by giving lower left x
;and y axis and upper right x and y axis values.

(struct bbox   (llx lly rux ruy) #:transparent)

; struct representing the vector of a particle in 2D space.

(struct vec (x y) #:transparent)

;Global declarations

(define theta 2)     ;decides which particles are deemed close to one another
(define iter 10000)    ;decides number of iterations to run
(define g 17.67398)    ;gravitational constant in UniverseX
(define timeslice .01) ;the simulation is updated after unit time
(define drawtime 1)    ;Draw every drawtime iterations

(define (zipwith f l1 l2)
  (cond [(or (null? l1) (null? l2)) ;
        [else (cons (f (car l1) (car l2)) (zipwith f (cdr l1) (cdr l2)))]))

(define (singleton l) ( = (length l) 1))

(define (sum l) (foldl + 0 l))
(define (concat l) (foldr append `() l))

(define (bounding-box particles)
  (define (fn-upper nextp acc) (max (vec-x nextp) (vec-y nextp) acc))
  (define (fn-lower nextp acc) (min (vec-x nextp) (vec-y nextp) acc))
  (let ([ur (+ 1 (foldr fn-upper -50000000 (map particle-posn particles)))]
        [ll (- (foldr fn-lower  50000000 (map particle-posn particles)) 1)])
    (bbox ll ll ur ur )))

(define-syntax lc
  (syntax-rules (: <- *)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : * guard) (if guard (list expr) `())]
    [(lc expr : * guard  qualifier ...) 
     (concat (lc (lc expr : qualifier ...) : guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (concat (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))