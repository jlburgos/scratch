; Practicing with Racket for the first time.
#lang racket

; make things public
(provide (all-defined-out))

(define x 3) ; val x = 3
(define y (+ x 2)) ; + is a funciton, call it here

(define cube1 ; val binding to anonymous function lambda(x)
  (lambda (x) ; anonymous function that takes one arg (x)
    (* x (* x x)))) ; <- this body is x^3

(define cube2
  (lambda (x)
    (* x x x)))

; Some syntactic sugar for defining variable cube3 to be bound to
; function (* x x x)
(define (cube3 x)
  (* x x x))

; Now for some recursive functions
(define (pow1 x y) ; x to the yth power (y must be non-negative)
  ; if-else are written as: (if e1 e2 e3)
  (if (= y 0) 
      1
  (* x (pow1 x (- y 1)))))

; Curried version of pow1
(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))

; partially apply pow2
(define three-to-the (pow2 3))