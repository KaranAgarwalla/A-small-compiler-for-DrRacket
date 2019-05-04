#lang racket

(provide (all-defined-out))
(require "defs.rkt")

;(provide (struct-out pgm)(struct-out def)(struct-out defexp)(struct-out uexp)
;         (struct-out bexp )(struct-out uexp)(struct-out iff)
;         (struct-out app)(struct-out lam)(struct-out sett)(struct-out lett)
;         (struct-out lets)(struct-out beginexp))


;(struct pgm (deflist) #:transparent)
;(struct def (var/fun exp) #:transparent)

;A variable is an expression
;A constant is an expression
;(struct defexp (deflist exp) #:transparent) 
;(struct uexp (op exp) #:transparent)        ;op = car, cdr
;(struct bexp (op exp1 exp2) #:transparent)  ;op = cons, +, -, *, <, =, <=
;(struct iff (cond exp1 exp2) #:transparent)
;(struct app (fun explist) #:transparent) 
;(struct lam (varlist exp) #:transparent)
;(struct sett (var exp) #:transparent)
;(struct lett (deflist exp2) #:transparent)
;(struct lets (deflist exp2) #:transparent)
;(struct beginexp (explist) #:transparent)



;The example program
;  (define (f g x) (g (* x x))) 
;  (define x 4)
;  (define (h y) (+ x y))
;  (define main (f h 5))

;;;;;;;;;;;;;;;;;

(define prog1
  (pgm (list
        (def 'f (lam (list 'g 'x) (app 'g (list (bexp * 'x 'x)))))
        (def 'x 4)
        (def 'h (lam (list 'y) (beginexp (list (debugexp) (bexp + 'x 'y)))))
        (def 'main (app  'f (list 'h 5))))))

;;;;;;;;;;;;;;;

(define prog2
  (pgm (list
        (def 'fact (lam (list 'n)
                       (beginexp
                         (list 
                          (debugexp)
                          (iff (bexp = 'n 0)
                             1
                             (bexp * 'n
                              (app 'fact
                                   (list (bexp - 'n 1)))))))))
        (def  'main (app 'fact (list 50))))))

;;;;;;;;;;;;;;;;

(define prog3
    (pgm (list
        [def 'length (lam (list 'l)
                        (iff (uexp null? 'l)
                             0
                             (bexp + 1
                              (app 'length
                                   (list (uexp cdr 'l))))))]
        [def  'res (app 'length (list (list 1 2 3 4 5 6 7 8 9 0)))]
        [def  'main (sett 'res (bexp + 'res 1))])))

;;;;;;;;;;;;;;;

(define prog4
  (pgm (list
        [def 'main (lets (list (def 'a 1)
                               (def 'b (bexp + 'a 1))
                               (def 'somelambda (lam '() 'c))
                               (def 'debug
                                 (beginexp
                                   (list
                                    (debugexp))))

                                 
                               (def 'c (bexp + 'a 'b)))
                         (bexp + 'a (app 'somelambda '())))])))


;;;;;;;;;;;;;;;

;(define (make-account balance) 
;   (lambda  (amount)
;      (if (>= balance amount) 
;          (begin
;             (set! balance (- balance amount))
;             balance)
;          "Insufficient funds")))
;
;(define my-account (make-account 50))
;(define your-account (make-account 1000))
;> (my-account 20)
; 30
;> (your-account 20)
; 980
;> (my-account 50)
; "Insufficient funds"

(define prog5
 (pgm (list
       [def 'make-account
         (lam (list 'balance) 
              (lam (list 'amount)
                   (iff (bexp >= 'balance 'amount) 
                        (beginexp
                         (list 
                          (sett 'balance
                                (bexp - 'balance 'amount))
                          'balance))
                       "Insufficient funds")))]
       [def 'my-account (beginexp
                          (list
                           (debugexp)
                           (app 'make-account (list 50))))]
       [def 'your-account (app 'make-account (list 1000))]
       [def 'main1 (app 'my-account (list 20))]
       [def 'main (beginexp
                          (list
                           (debugexp)
                           (app 'your-account (list 20))))])))

;(define (p q x) (q x))
;(define (s x)
;  (define (t y) (+ x y))
;  (p t 5))
;(define (main) (s 4))
;(main)



(define prog6
  (pgm (list
        [def 'p (lam (list 'q 'x) (beginexp (list (debugexp) (app 'q (list 'x)))))]
        [def 's (lam (list 'x)
          (defexp (list
                   [def 't (lam (list 'y) (bexp + 'x 'y))])
            (app 'p (list 't 5))))]
        [def 'main (app 's (list 4))])))

;(define (recurse i q)
;  (define (p) (display i))
;  (if (> i 0) (recurse (- i 1) p)
;      (begin # (p) (q))))
;(define (dummy) (display ""))
;(define (main) (recurse 1 dummy))
;(main)

(define prog7
  (pgm (list
        [def 'recurse (lam (list 'i 'q)
                           (defexp (list
                                    [def 'p (lam '() (uexp displayln  'i))])
                             (iff (bexp > 'i 0) (app 'recurse (list (bexp - 'i 1) 'p))
                                  (beginexp (list (beginexp (list (debugexp) (app 'p '())))
                                                  (app 'q '()))))))]
        [def 'dummy (lam '() (uexp display ""))]
        [def 'mainfun (lam '() (app 'recurse (list 1 'dummy)))]
        [def 'main (app 'mainfun '())]
)))
        
;(define x 4)
;(define (f x)
;  (let [(g (lambda (y) #(+ x y)))]
;    (lambda (x) (if (= x 3) (g 5) ((f (- x 1)) 3)))))
;(define w (f 3))
;(define result (w 4))

(define prog8
  (pgm (list
        [def 'x 4]
        [def 'f (lam (list 'x)
                     (lett (list [def 'g (lam (list 'y) (beginexp (list (debugexp)
                                                                        (bexp + 'x 'y))))])
                           (lam (list 'x) (iff (bexp = 'x 3) (app 'g (list 5))
                                               (app (app 'f (list (bexp - 'x 1))) (list 3))))))]
        [def 'w (app 'f (list 3))]
        [def 'main (app 'w (list 4))])))


;(define (s x) (if (= x 5) 2 -1))
;(define main (begin
;               (set! s (let ((g s)) (lambda (x) # (if (= x 6) 1 (g x)))))
;               (s 6)))

(define prog9
  (pgm (list
        [def 's (lam (list 'x) (iff (bexp = 'x 5) 2 -1))]
        [def 'main (beginexp
                     (list (sett 's
                                  (lett (list (def 'g 's))
                                        (lam (list 'x)
                                             (beginexp (list (debugexp)
                                                             (iff (bexp = 'x 6) 1
                                                                  (app 'g (list 'x))))))))
                           (app 's (list 6))))])))
                     
;(define (s x) (if (= x 5) 2 -1))
;(define main (begin
;               (set! s (lambda (x) (if (= x 6) 1 (s x))))
;               (s 6)))


(define prog10
  (pgm (list
        [def 's (lam (list 'x) (beginexp (list (debugexp) (iff (bexp = 'x 5) 2 -1))))]
        [def 'main (beginexp
                     (list (sett 's
                                  (lam (list 'x)
                                       (beginexp (list (debugexp)
                                                       (iff (bexp = 'x 6) 1 (app 's (list 'x)))))))
                           (app 's (list 7))))])))


(define prog11
   (pgm (list
        [def  'a 1]
	[def  'b 2]
        [def 'main (beginexp (list (debugexp) (bexp + 'a 'b)))])))

(define prog12
   (pgm (list
        [def  'a 1]
	[def  'b 2]
	[def 'c (lam '() (beginexp (list (debugexp) (bexp + 'a 'b))))]	
        [def 'main (app 'c '())])))