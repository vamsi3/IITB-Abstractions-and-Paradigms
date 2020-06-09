#lang racket

(provide connect-ends)

;; The facility to plot a curve.

(require plot)
(plot-new-window? #t)
(plot-width 900)
(plot-height 900)
(define (draw curve)
  (plot (parametric
         (lambda (t) (vector
                      (x-of (curve t))
                      (y-of (curve t))))
         0 1 #:width 1 #:samples 20000
         #:x-min -10.5 #:x-max 10.5
         #:y-min -10.5 #:y-max 10.5)))

;plot-width and plot-height give the size of the window
;#:width refers to the thickness of the line
;#:samples is the number of sample points. decrease it if
;your program takes too much time
;#:x-min -1 #:x-max 2 says that the graph is plotted in
;the x-axis range -1 to 2

;; Defining the 'make-point' constructor and 'x-of' , 'y-of' selectors.

(define (make-point x y)
  (lambda (bit)
    (if (zero? bit) x y)))

(define (x-of point)
  (point 0))

(define (y-of point)
  (point 1))

;; Retained Code - Answer to Question-2(b)

(define (translate x y curve)
  (lambda (t)
    (let [(ct (curve t))]
      (make-point (+ x (x-of ct)) (+ y (y-of ct))))))

;; Answer to Question-3

(define (connect-ends curve1 curve2)
  (let* [(p1 (curve1 1))
         (p2 (curve2 0))]
    (lambda (t)
      (if (< t (/ 1 2))
          (curve1 (* 2 t))
          ((translate (- (x-of p1) (x-of p2)) (- (y-of p1) (y-of p2)) curve2) (- (* 2 t) 1))))))