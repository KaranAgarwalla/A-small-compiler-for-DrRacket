#lang racket
(require racket/struct)
(provide (all-defined-out))
(provide (struct-out pgm)(struct-out defexp)(struct-out uexp)
         (struct-out bexp )(struct-out uexp)(struct-out iff)
         (struct-out app)(struct-out lam)(struct-out sett)(struct-out lett)
         (struct-out lets) (struct-out frame)  (struct-out closure)
         (struct-out emptyframe))


(struct pgm (deflist) #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) "pgm")
      (lambda (obj) (list
                     
                     (print-deflist (pgm-deflist obj))))))])



(struct def (var/fun exp) #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) (format "~a"  "def"))
      (lambda (obj) (list
                     (format "~a"  (def-var/fun obj))
                     (format "~a"  (def-exp obj))))))])

(struct uexp (op exp) #:transparent       ; op = car, cdr
 #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) (format "~a"  (getsym (uexp-op obj))))
      (lambda (obj) (list
                     (format "~a"  (uexp-exp obj))))))])
                   

(struct bexp (op exp1 exp2) #:transparent ; op = cons, +, -, *, <, =, <=
 #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) (format "~a" (getsym (bexp-op obj))))
      (lambda (obj) (list
                     (format "~a"  (bexp-exp1 obj))
                     (format "~a"  (bexp-exp2 obj))))))])





(struct iff (cond exp1 exp2) #:transparent
 #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) "iff")
      (lambda (obj) (list
                     (format "~a"  (iff-cond obj))
                     (format "~a"  (iff-exp1 obj))
                     (format "~a"  (iff-exp2 obj))))))])

(struct app (fun explist) #:transparent
 #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) "app")
      (lambda (obj) (list
                     (format "~a"  (app-fun obj))
                     (format "~a"  (app-explist obj))))))])

(struct lam (varlist exp) #:transparent
 #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) "lam")
      (lambda (obj) (list
                     (format "~a"  (lam-varlist obj))
                     (format "~a"  (lam-exp obj))))))])
      

(struct sett (var exp) #:transparent)
(struct lett (deflist exp) #:transparent)
(struct lets (deflist exp) #:transparent)
(struct debugexp ())                    ; I shall use it to check your answers
(struct beginexp (explist) #:transparent
 #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) (format "~a"  "beginexp"))
      (lambda (obj) (list
                     (format "~a"  (beginexp-explist obj))))))])



(struct defexp (deflist exp) #:transparent)




(struct closure (lambda frame) #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) "")
      (lambda (obj) (list
                     (format "~a" (closure-lambda obj))
                     "Environment: "
                     (let ([fp (closure-frame obj)])
                       (if (emptyframe? fp)
                           fp
                           (format "~a" (frame-number fp))))))))])

(struct frame (number bindings parent) #:transparent
  #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) "Frame")
      (lambda (obj) (list
                     "Number:"
                     (format "~a\n" (frame-number obj))
                     "Bindings:"
                     (print-bindings (frame-bindings obj))
                     "\n"
                     "Parent: "
                     (let ([fp (frame-parent obj)])
                       (if (emptyframe? fp)
                           (format "~a" (emptyframe))
                           (format "~a" (frame-number fp)))

                       )))))]
  )

(struct emptyframe() #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) "emptyframe")
      (lambda (obj) (list ""))))]
  )

(define (print-deflist deflist)
 (match deflist
      ['() ""]
      [(cons (def name definition) l) (string-append
                             (format "~n")
                             (format "  ~a --> ~a" name definition)
                           
                             (print-deflist l))]))  

(define (print-bindings ht)
  (define (print-list htl)
    (match htl
      ['() ""]
      [(cons (cons x xs) l) (string-append
                             (format "~n   ~a --> ~a" x xs)
                             (print-list l))]))
  (print-list (hash->list ht)))

(define (getsym entity)
  (cond
    [(equal? entity +) "+"]
    [(equal? entity -) "-"]
    [(equal? entity *) "*"]
    [(equal? entity /) "/"]
    [(equal? entity car) "car"]
    [(equal? entity cdr) "cdr"]
    [(equal? entity cons) "cons"]
    [(equal? entity =) "="]
    [(equal? entity <) "<"]
    [(equal? entity <=) "<="]
    [(equal? entity null?) "null?"]))

