#lang racket
(require racket/struct)
(require "defs.rkt")
(require "examples.rkt")
(require "model-interpreter.rkt")



(displayln "Program 1*******************************\n")
;
(displayln prog1)
;
(displayln "\n\nProgram evaluation ********************************\n")

(eval-program prog4)

;;and so on 