#lang racket
(provide (all-defined-out))

; 1 1 1 1 1 1 1 ...
(define ones (lambda () (cons 1 ones)))

(define (f x) (cons x (lambda () (f (+ x 1)))))

(define nats (lambda () (f 1)))

(define naturals
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;(define (stream-maker fn arg) ...)