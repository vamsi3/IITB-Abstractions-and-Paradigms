#lang racket
(require "declarations.rkt")
;(require "testcases.rkt")
(require racket/gui)

(provide draw-particles)

(define frame-size 750)
(define bitmap-size 750)

  ; Make a 500 x 500 frame
(define frame (new frame% [label "N-body movement simulation"]
                   [width frame-size]
                   [height frame-size]))

; Make the drawing area with a paint callback
(define canvas
  (new canvas% [parent frame]
       [paint-callback
        (lambda (canvas dc) (paint dc))]))

; ... pens, brushes, and draw-face are the same as above ...

(define (paint dc) (send dc draw-bitmap face-bitmap 0 0))

; ... pens, brushes, and draw-face are the same as above ...

; Create a bitmap
(define face-bitmap (make-object bitmap% bitmap-size bitmap-size ))
; Create a drawing context for the bitmap
(define bm-dc (make-object bitmap-dc% face-bitmap))
; A bitmap's initial content is undefined; clear it before drawing
(send bm-dc clear)

; Make some pens and brushes
(define black-pen (make-object pen% "BLACK" 1 'solid))
(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-pen (make-object pen% "RED" 2 'solid))

;;Change this to get object sizes to your liking

(define scale-radius 2)


; Show the frame
(send frame show #t)
  ;  draw-particles :: [(Radius, Posn)] -> Action
(define (draw-particles l)
  (begin
    (send bm-dc clear)
    (send bm-dc set-brush yellow-brush)
    (send bm-dc set-pen red-pen)
    (map (lambda (p) (let*
                         ([posn (particle-posn p)]
                          [diameter (* 2  scale-radius (expt (particle-mass p) .3333))]
                          [x (- (vec-x posn) (/ diameter 2))]
                          [y (- (- bitmap-size (vec-y posn)) (/ diameter 2))]
                          )
                       (send bm-dc draw-ellipse x y diameter diameter))) l)
    (send canvas refresh)
    (sleep/yield 0.01)))

