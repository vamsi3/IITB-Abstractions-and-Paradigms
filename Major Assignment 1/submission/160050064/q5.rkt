#lang racket

(provide gosper-curve)

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

;; Retained Code - Answer to Question-2(d)

(define (rotate-around-origin radians curve)
  (lambda (t)
    (let* [(ct (curve t))
           (cosr (cos radians))
           (sinr (sin radians))
           (xct (x-of ct))
           (yct (y-of ct))]
      (make-point (- (* cosr xct) (* sinr yct)) (+ (* sinr xct) (* cosr yct))))))

;; Retained Code - Answer to Question-2(e)

(define (put-in-standard-position curve)
  (define temp-translate
    (let [(c (curve 0))]
      (translate (- (x-of c)) (- (y-of c)) curve)))
  (let* [(end1 (temp-translate 1))
         (x (x-of end1))
         (y (y-of end1))]
    (define temp-rotate
      (if (= x 0) (cond [(> y 0) (rotate-around-origin (/ pi -2) temp-translate)]
                        [else (rotate-around-origin (/ pi 2) temp-translate)])
          (if (= y 0) (cond [(> x 0) temp-translate]
                            [else (scale -1 -1 temp-translate)])
              (let [(angle-minus (- (asin (/ y (sqrt (+ (* x x) (* y y)))))))]
                (cond [(> x 0) (rotate-around-origin angle-minus temp-translate)]
                      [else (rotate-around-origin (+ pi angle-minus) temp-translate)])))))
    (if (and (= x 0) (= y 0)) temp-translate
        (let ((rot (/ 1 (x-of (temp-rotate 1)))))
          (scale rot rot temp-rotate)))))

;; Retained Code - Answer to Question-3

(define (connect-ends curve1 curve2)
  (let* [(p1 (curve1 1))
         (p2 (curve2 0))]
    (lambda (t)
      (if (< t (/ 1 2))
          (curve1 (* 2 t))
          ((translate (- (x-of p1) (x-of p2)) (- (y-of p1) (y-of p2)) curve2) (- (* 2 t) 1))))))

;; Retained Code - Answer to Question-4

(define (gosper-step curve)
  (put-in-standard-position (connect-ends (rotate-around-origin (/ pi 4) curve) (rotate-around-origin (- (/ pi 4)) curve))))

;; Answer to Question-5

(define (gosper-curve level)
  ((repeated gosper-step level) (unit-line)))
