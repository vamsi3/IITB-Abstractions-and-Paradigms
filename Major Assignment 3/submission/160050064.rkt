#lang racket

(provide combine-cc combine-sc combine-cs combine-ss)
(provide pred-p)
(provide single-digit-p)
(provide single-alphabet-p)
(provide seq)
(provide alt)
(provide epsilon-p)
(provide zero-or-more)
(provide one-or-more)
(provide whitespace-p)
(provide number-p)
(provide identifier-p)
(provide variable-p)
(provide term-p)
(provide expression-p)
(provide assignment-p)

(define-struct gnode (sym list) #:transparent)
(define-struct ident (str) #:transparent)
(define-struct num (val) #:transparent)

(define (combine-cc char1 char2)
  (list->string (list char1 char2)))

(define (combine-sc str char)
  (list->string (append (string->list str)
                        (list char))))

(define (combine-cs char str)
  (list->string (cons char (string->list str))))

(define (combine-ss str1 str2)
  (list->string (append (string->list str1)
                        (string->list str2))))

(define (pred-p p)
  (λ (str)
    (if (equal? str "") (cons "" "")
        (let ((first-char (string-ref str 0)))
          (if (p first-char) (cons first-char (substring str 1))
              'fail)))))

(define single-digit-p
  (pred-p (lambda (char)
            (if (and (char>=? char #\0) (char<=? char #\9)) #t #f))))

(define single-alphabet-p
  (pred-p (lambda (char)
            (if (and (char-ci>=? char #\a) (char-ci<=? char #\z)) #t #f))))

(define (seq p1 p2 f) (lambda (str)
                        (if (equal? str "") 'fail
                        (let [(first-parse (p1 str))]
                          (if (eq? first-parse 'fail) 'fail
                              (let [(second-parse (p2 (cdr first-parse)))]
                                (if (eq? second-parse 'fail) 'fail
                                    (cons (f (car first-parse) (car second-parse)) (cdr second-parse)))))))))

(define (alt p1 p2) (lambda (str)
                      (if (equal? str "") 'fail
                      (let [(first-parse (p1 str))]
                        (if (eq? first-parse 'fail) (p2 str)
                            first-parse)))))

(define epsilon-p (lambda (str) (cons "" str)))

(define (zero-or-more p f)
  (λ (str)
    (if (equal? str "") (cons "" "")
        (let [(first (p str))]
          (if (eq? first 'fail) (epsilon-p str)
              (let [(next ((zero-or-more p f) (cdr first)))]
                (cons (f (car first) (car next)) (cdr next))))))))

(define (one-or-more p f)
  (λ (str)
    (if (equal? str "") (cons "" "")
        (let [(first (p str))]
          (if (eq? first 'fail) 'fail
              (let [(next ((zero-or-more p f) (cdr first)))]
                (cons (f (car first) (car next)) (cdr next))))))))

(define whitespace-p
  (λ (str)
    (if (equal? str "") (cons "" "")
        (if (char-whitespace? (string-ref str 0)) (whitespace-p (substring str 1))
            (epsilon-p str)))))

(define number-p
  (λ (str)
    (let [(simplified ((one-or-more single-digit-p combine-cs) (cdr (whitespace-p str))))]
      (cond [(equal? simplified 'fail) 'fail]
            [(equal? simplified (cons "" "")) simplified]
            [else (cons (num (string->number (car simplified))) (cdr simplified))]))))

(define identifier-p
  (λ (str)
    ((seq single-alphabet-p
          (zero-or-more (alt single-alphabet-p single-digit-p) combine-cs)
          (λ (x y) (if (equal? y "") (begin (when (char? x) (set! x (string x))) (ident x))
                       (ident (combine-cs x y))))) (cdr (whitespace-p str)))))

(define (variable-p str)
  (define (box-open-p str) ((pred-p (λ (c) (char=? c #\[))) (cdr (whitespace-p str))))
  (define (box-close-p str) ((pred-p (λ (c) (char=? c #\]))) (cdr (whitespace-p str))))
  (define c1 (λ (x y) x)) 
  (define c2 (λ (x y) y))
  (define c3 (λ (x y) (gnode 'ARRAY (list x y))))
  ((alt (seq identifier-p (seq box-open-p (seq expression-p box-close-p c1) c2) c3)
        identifier-p) (cdr (whitespace-p str))))

(define (term-p str)
  (define (paran-open-p str) ((pred-p (λ (c) (char=? c #\())) (cdr (whitespace-p str))))
  (define (paran-close-p str) ((pred-p (λ (c) (char=? c #\)))) (cdr (whitespace-p str))))
  (define c1 (λ (x y) x)) (define c2 (λ (x y) y))
  ((alt (alt number-p variable-p)
        (seq paran-open-p (seq expression-p paran-close-p c1) c2)) (cdr (whitespace-p str))))

(define (expression-p str)
  (define (plus-p str) ((pred-p (λ (c) (char=? c #\+))) (cdr (whitespace-p str))))
  (define c1 (λ (x y) y)) (define c2 (λ (x y) x))
  (define c3 (λ (x y) (if (equal? y "") x
                          (gnode 'PLUS (list x y)))))
  ((seq term-p (zero-or-more (seq plus-p expression-p c1) c2) c3) (cdr (whitespace-p str))))

(define (assignment-p str)
  (define (equalto-p str) ((pred-p (λ (c) (char=? c #\=))) (cdr (whitespace-p str))))
  (define c1 (λ (x y) y))
  (define c2 (λ (x y) (gnode 'ASSIGN (list x y))))
  ((seq variable-p (seq equalto-p expression-p c1) c2) (cdr (whitespace-p str))))
