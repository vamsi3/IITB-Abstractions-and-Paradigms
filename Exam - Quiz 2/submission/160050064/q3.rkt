#lang racket

(require racket/mpair)
(provide choose memo-choose propagate the-queue)
(define incr%
;;...
) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define delay%
;;...
) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define multiplier%
;;...
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define wire%
  (class object%
    (init-field name)
    (init-field (signal-value 'undefined))
    (init-field (actions '()))

    (super-new)

    (define/public (get-signal) signal-value)
    (define/public (get-name) name) 

    (define/public (set-signal! new-value)
      (cond [(not (equal? new-value signal-value))
             (begin
               (set! signal-value new-value)
               (call-each actions))]))

    (define (call-each actions)
      (cond [(not (null? actions))
             (begin
               ((car actions))
               (call-each (cdr actions)))]))
    
    (define/public (add-action! action)
      (set! actions (cons action actions)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define event-queue%
  (class object% 
    (init-field (current-time 0))
    (init-field (farthest-event-time 0))
    (init-field (queue (make-vector 20 '())))
    
    (super-new)   
    
    (define/public (add-to-queue! time action)
      (begin
        (vector-set! queue time
                     (mappend! (vector-ref queue time)
                               (mlist action)))
        (cond ((> time farthest-event-time)
               (set! farthest-event-time time)))))
      
    (define/public (queue-empty?)
      (> current-time farthest-event-time))
    
    (define/public (increment-time!)
      (set! current-time (+ current-time 1)))
    
    (define/public (next-item-in-queue)
      (vector-ref queue current-time))
    
    (define/public (get-time)
      current-time)))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
(define monitor% 
  (class object%
    (init-field wires)
    (super-new)
    (define/public (addwires wires1)
      (set! wires (append wires wires1)))
    (define/public (report) (report-helper wires))
    (define (report-helper wires)
      (cond [(null? wires) (void)]
            [else 
             (begin
               (newline)
               (display "time = ")
               (display (send the-queue get-time ))
               (display ", wire-name = ")
               (display (send (car wires) get-name))
	       (display ", New-value = ")
	       (display (send (car wires) get-signal))
	       (report-helper (cdr wires)))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(define (propagate queue)
  (if (send the-queue queue-empty?)
      'done
      (let ((action-list (send the-queue next-item-in-queue)))
        (execute! action-list)
        (send queue increment-time!)
        (propagate queue))))

(define (execute! action-list)
  (cond [(not (null? action-list)) 
         (begin
           ((mcar action-list)) 
           (execute! (mcdr action-list)))]))


(define (add-to-queue-after delay action)
  (cond [(> (+ delay (send the-queue get-time)) 20) (void)]
        [else  (send the-queue add-to-queue! 
                     (+ delay (send the-queue get-time)) action)]))

(define the-queue (make-object event-queue%))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Do the connections shown in the diagram here.


(define the-monitor (make-object monitor% '()))
(send the-monitor addwires (list a b c d))
   
;; Set the initial value 


(propagate the-queue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;sample output, ignore the semicolon at the beginning:

;time = 3, wire-name = a, New-value = 4
;time = 3, wire-name = b, New-value = 5
;time = 3, wire-name = c, New-value = 24
;time = 3, wire-name = d, New-value = 6
;time = 4, wire-name = a, New-value = 5
;time = 4, wire-name = b, New-value = 6
;time = 4, wire-name = c, New-value = 120
;time = 4, wire-name = d, New-value = 24
;time = 5, wire-name = a, New-value = 6
;time = 5, wire-name = b, New-value = 7
;time = 5, wire-name = c, New-value = 720
;time = 5, wire-name = d, New-value = 120
;time = 6, wire-name = a, New-value = 7
;time = 6, wire-name = b, New-value = 8
;time = 6, wire-name = c, New-value = 5040
;time = 6, wire-name = d, New-value = 720
;...






