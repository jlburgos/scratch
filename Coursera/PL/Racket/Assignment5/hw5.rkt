;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;(require racket/trace) ;; for debugging purposes

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
;; Part (A)
(define (racketlist->mupllist rlst)
  (cond [(= (length rlst) 0) (aunit)]
        [#t (apair (car rlst) (racketlist->mupllist (cdr rlst)))]))

;; Part (B)
(define (mupllist->racketlist mlst)
  (cond [(apair? mlst) (cond [(aunit? (apair-e2 mlst)) (list (apair-e1 mlst))]
                             [#t (append (list (apair-e1 mlst)) (mupllist->racketlist (apair-e2 mlst)))])]
        [#t (error "mupllist->racketlist expected an apair or aunit")]))

;; Problem 2


;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) 
         e]
        [(mlet? e)
         (if (string? (mlet-var e))
             (let ([v1 (eval-under-env (mlet-e e) env)])
               (eval-under-env (mlet-body e) (append 
                                              (list (cons (mlet-var e) v1))
                                              env)))
                                              
             (error "MUPL mlet has non-string var"))]
        [(closure? e) 
         e]
        [(fun? e) 
         (closure (append (list fun-nameopt) env) e)]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (eval-under-env (fun-body (closure-fun v1))
                               (if (equal? (fun-nameopt (closure-fun v1)) #f)
                                   (append (list 
                                            (cons (fun-formal (closure-fun v1)) v2)
                                            (closure-env v1)))
                                   (append (list
                                            (cons (fun-nameopt (closure-fun v1)) v1) 
                                            (cons (fun-formal (closure-fun v1) v2))
                                           (closure-env v1)))))
               (error "MUPL call given non-closure as first argument" e)))]
        [(ifgreater? e) 
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e) 
         (let ([v1 (eval-under-env (fst-e e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e) 
         (let ([v1 (eval-under-env (snd-e e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "MUPL snd applied to non-pair")))]
        [(isaunit? e)
         (let ([v1 (eval-under-env (isaunit-e e) env)])
           (if (aunit? v1)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
;(trace eval-under-env) ;; for debugging purposes
        
;; Problem 3

(define (ifaunit e1 e2 e3) (if (aunit? e1) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) 
            (cdr (car lstlst)) 
            (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (ifgreater e1 e2 e4 (ifgreater e2 e1 e4 e3)))

;; Problem 4
; (struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
; (struct call (funexp actual)       #:transparent) ;; function call
(define mupl-map
    (letrec ([f1 (lambda (mupl-f)
                   (letrec ([f2 (lambda (mupl-lst)
                                  (ifaunit (mupl-lst)
                                           (aunit) 
                                           (cons (mupl-f (fst mupl-lst)) (f2 (snd mupl-lst)))))]) f2))]) f1))

(define mupl-map2
  (fun "



(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
