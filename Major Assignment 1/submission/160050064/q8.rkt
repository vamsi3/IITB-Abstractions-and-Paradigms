#lang racket

(provide sierpinski-curve)

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
         #:x-min -0.5 #:x-max 1.5
         #:y-min -0.5 #:y-max 1.5)))

;plot-width and plot-height give the size of the window
;#:width refers to the thickness of the line
;#:samples is the number of sample points. decrease it if
;your program takes too much time
;#:x-min -1 #:x-max 2 says that the graph is plotted in
;the x-axis range -1 to 2

;; Defining the 'repeated' function.

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (identity x) x)

(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

;; Defining the 'make-point' constructor and 'x-of' , 'y-of' selectors.

(define (make-point x y)
  (lambda (bit)
    (if (zero? bit) x y)))

(define (x-of point)
  (point 0))

(define (y-of point)
  (point 1))

;; Defining a (unit-line) from (0,0) to (1,0)

(define (unit-line)
  (lambda (t) (make-point t 0)))

;; Retained Code - Answer to Question-2(b)

(define (translate x y curve)
  (lambda (t)
    (let [(ct (curve t))]
      (make-point (+ x (x-of ct)) (+ y (y-of ct))))))

;; Retained Code - Answer to Question-2(c)

(define (scale x y curve)
  (lambda (t)
    (let [(ct (curve t))]
      (make-point (* x (x-of ct)) (* y (y-of ct))))))

;; Retained Code - Answer to Question-3

(define (connect-ends curve1 curve2)
  (let* [(p1 (curve1 1))
         (p2 (curve2 0))]
    (lambda (t)
      (if (< t (/ 1 2))
          (curve1 (* 2 t))
          ((translate (- (x-of p1) (x-of p2)) (- (y-of p1) (y-of p2)) curve2) (- (* 2 t) 1))))))

;; Answer to Question-8

(define (unit-triangle)
  (lambda (t)
    (cond [(< t (/ 1 3)) ((unit-line) (* 3 t))]
          [(< t (/ 2 3)) (make-point (/ (* 3 (- 1 t)) 2) (/ (* (- (* 3 t) 1) (sqrt 3)) 2))]
          [else (make-point (/ (* 3 (- 1 t)) 2) (/ (* 3 (- 1 t) (sqrt 3)) 2))])))

(define (sierpinski-curve level)
  ((repeated sierpinski-step level) (unit-triangle)))

(define (sierpinski-step curve)
  (let [(c2 (scale 0.5 0.5 curve))]
    (lambda (t)
      (cond [(< t (/ 1 3)) (c2 (* 3 t))]
            [(< t (/ 2 3)) ((translate (/ 1 4) (/ (sqrt 3) 4) c2) (- (* 3 t) 1))]
            [(< t (/ 17 18)) ((translate (/ 1 2) 0 c2) (/ (- (* 18 t) 12) 5))]
            [else (make-point (- 18 (* 18 t)) 0)]))))