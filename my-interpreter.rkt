#lang racket
(require racket/struct)
(provide (all-defined-out))
(require racket/hash)
(require "defs.rkt")
(require "examples.rkt")

;;Global definitions. A counter that tracks the framenumber
(define framenumber 0)

;The stack and its operations. I have decided to make the stack a global.
(define stack '())
(define (push frame) (set! stack (cons frame stack)))
(define (pop) (if (null? stack) (error "Empty stack")
                        (set! stack (cdr stack))))
(define (top) (if (null? stack) (error "Empty stack")
                        (car stack)))


;createframe creates a new frame. It gets its number from
;;framenumber and as a side effect increases framenumber
(define (createframe hashtable parent) ;hastable gives the initial bindings
   (frame (begin (set! framenumber (+ framenumber 1)) (- framenumber 1)) hashtable parent))

;This creates the global frame, which, in our case, contains
;empty bindings.
(push (createframe (make-hash '()) (emptyframe)))

;This interprets a program. It uses the following function processdef.
(define (eval-program prog)
         (match prog
           [(pgm deflist) (begin (map (lambda (x) (processdef x (top))) deflist) (return-value-of-main (top)))]))

;;processdef describes how each definition is processed and added to
;;the frame fr.
(define (processdef defn fr)
  (match defn    
    [(def v/f exp) (hash-set! (frame-bindings fr) v/f (eval-exp exp))]))

;; We have said that the result of the program is the value of
;; the symbol main. main must be a defined symbol in each program.
(define (return-value-of-main frame)
  (hash-ref! (frame-bindings frame) 'main "main not found"))

;; The expression evaluator is the heart of the interpreter.
;; It will use the functions below
(define (eval-exp exp)
  (cond [(symbol? exp) (hash-ref (frame-bindings (search exp (top))) exp "Symbol not found")]
        [(boolean? exp) exp]
        [(number? exp) exp]
        [(list? exp) exp]
        [(string? exp) exp]
        [else (match exp
                [(uexp op exp1) (op (eval-exp exp1))]
                [(bexp op exp1 exp2) (op (eval-exp exp1) (eval-exp exp2))]
                [(lam var exp) (closure (lam var exp) (top))]
                [(app exp1 explist) (let ([clo (eval-exp exp1)])
                                           (begin (push (createframe (update-hash-table (make-hash '()) (lam-varlist (closure-lambda clo)) explist)
                                                                     (closure-frame clo)))
                                                  (begin0 (eval-exp (lam-exp (closure-lambda clo))) (pop))))] 
                 ;(begin (push (createframe
                ;                            (make-hash [(map (lambda (x) (list x
                ;                             (hash-ref (frame-bindings (search x (top))) x))) exp1)]) (top))) (let ([x (eval-exp explist)])
                 ;                       (begin (pop) x)))]
                [(iff cond exp1 exp2) (if (eval-exp cond) (eval-exp exp1) (eval-exp exp2))]
                [(beginexp explist) (process-beginexp explist)]
                [(lets deflist exp) (process-lets deflist exp)]
                [(lett deflist exp) (process-lett deflist exp)]
                [(sett var exp) (hash-set! (frame-bindings (search var (top))) var (eval-exp exp))]
                [(defexp deflist exp) (begin (map (lambda (x) (processdef x (top))) deflist) (eval-exp exp))]
               ; [(; so on, fill in these...
                [(debugexp) (begin
                              (print-current-environment (top))
                              )])]))

;;;An auxilliary function that processes a begin expression
(define (update-hash-table hash-table list1 list2)
  (cond [(null? list1) hash-table]
        [else (begin (hash-set! hash-table (car list1) (eval-exp (car list2))) (update-hash-table hash-table (cdr list1) (cdr list2)))]))

(define (process-beginexp explist)
  (match explist
    [(cons exp '()) (eval-exp exp)]
    [(cons exp exp1list) (begin (eval-exp exp) (process-beginexp exp1list))]))
;
;;;An auxilliary function that processes a let expression.
;;;The let definitions are in deflist, and the let body is exp.
(define (process-lets deflist exp)
  (match deflist
    [(cons e1 '()) (process-lett deflist exp)]
    [(cons exp1 explist) (begin (define extraframe (createframe (make-hash '()) (top))) (processdef exp1 extraframe) (push extraframe)
                                (begin0 (process-lets explist exp) (pop)))]))

(define (process-lett deflist exp)
  (begin (define extraframe (createframe (make-hash '()) (top))) (map (lambda (x) (processdef x extraframe)) deflist) (push extraframe)
         (let ([ans (eval-exp exp)]) (begin (pop) ans))))

;;Prints the current environment running through a chain of frames.
;;Note that my struct definitions are such that if fr is a frame,
;;then (displayln fr) will print the frame in the format that I have
;;shown. 
(define (print-current-environment fr)
  (if (emptyframe? fr) (displayln "@@@@@@@@@@@@@@@@@@@@@@@")
      (begin (displayln "@@@@@@@@@@@@@@@@@@@@@@@")
             (displayln fr)
             (print-current-environment (frame-parent fr)))))
  
;;Search for the symbol sym in an environment that starts with the frame
;;fr. We shall make search return either the  emptyframe
;;or the frame containing the symbol (note, not the value of the
;;symbol itself.

(define (search sym fr)
  (if (emptyframe? fr) (error "symbol not found")
  (if (hash-has-key? (frame-bindings fr) sym)
      fr
      (search sym (frame-parent fr)))))

(define (drop-last l)
  (cond [(null? (cdr l)) '()]
        [else (cons (car l) (drop-last (cdr l)))]))


