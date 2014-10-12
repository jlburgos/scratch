#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Debug methods
;(define (fff x) (cons x (lambda () (fff (+ x 1)))))
;(define nats (lambda () (fff 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
(define (sequence low high stride)
  (letrec ([f (lambda (val lst)
                (cond [(< high val) lst]
                      [#t (f (+ val stride) (cons val lst))]))])
    (reverse (f low null))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
(define (list-nth-mod xs n)
  (cond 
    [(negative? n) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t (car (list-tail xs (remainder n (length xs))))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 4
(define (stream-for-n-steps s n)
  (letrec ([f (lambda (step lst ss)
                (cond [(> step n) lst]
                      [#t (f (+ 1 step) (cons (car (ss)) lst) (cdr (ss)))]))])
    (reverse (f 1 null s))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cond [(equal? (remainder x 5) 0) 
                       (cons (- x) (lambda () (f (+ 1 x))))]
                      [#t (cons x (lambda () (f (+ 1 x))))]))])
    (lambda () (f 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 6
(define dan-then-dog
  (letrec ([f (lambda (x)
                (cond [(equal? "dog.jpg" x) (cons x (lambda () (f "dan.jpg")))]
                      [#t (cons x (lambda () (f "dog.jpg")))]))])
    (lambda () (f "dan.jpg"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 7
(define (stream-add-zero s)
  (lambda () (cons 
              (cons 0 (car (s))) 
              (stream-add-zero (cdr (s))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 8
(define (cycle-lists xs ys)
  (letrec ([inc-num (lambda (i len)
                      (cond [(equal? (+ 1 i) len) 0]
                            [#t (+ 1 i)]))]
           [f (lambda (xi yi)
                (cons (cons (car (list-tail xs xi)) 
                            (car (list-tail ys yi)))
                 (lambda () (f (inc-num xi (length xs)) 
                               (inc-num yi (length ys))))))])
    (lambda () (f 0 0))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (pos)
                (cond [(equal? (vector-length vec) pos) #f]
                      [(not (pair? (vector-ref vec pos)))
                       (f (+ 1 pos))]
                      [#t (cond [(equal? (car (vector-ref vec pos)) v) 
                                 (vector-ref vec pos)]
                                [#t (f (+ 1 pos))])]))])
    (f 0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [pos 0]
           [inc-num (lambda (i)
                      (cond [(equal? (+ 1 i) n) 0]
                            [#t (+ 1 i)]))])
    (lambda (v) ; client-side method
      (let ([found (vector-assoc v cache)])
        (cond [found found]
              [#t (let ([new-value (assoc v xs)]) ; find in list xs
                      (if new-value ; if found return value, else false
                          (begin
                            (vector-set! cache pos new-value)
                            (set! pos (inc-num pos))
                            new-value)
                          #f))])))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;