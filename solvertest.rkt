#lang racket/base

(require "Solver.rkt"
         "Grid.rkt"
         racket/class)


(define g (new grid%))
(send g debug-grid)
(send g print #f)