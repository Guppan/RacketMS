#lang racket/base

(require "Solver.rkt"
         "Grid.rkt"
         racket/class)

(define out (open-output-file "debug.txt" #:exists 'truncate))
(define g (new grid%))
(send g debug-grid)
(send g print #t out)

(define solv (new solver% [grid g]))
(send solv set-size! (cons 5 5))

(define start-pos (cons 0 4))
(send solv mark-add-sentence! start-pos (send g get-value start-pos))

(send solv print-info out)

(send solv update-knowledge-loop!)
(send solv remove-filter-empty-sentence!)
(send solv conclude-new-knowledge!)
(send solv update-knowledge-loop!)
(send solv remove-filter-empty-sentence!)

(for ([i (in-range 17)])
  (send solv tick-solver!)
  (send g print #f out)
  (send solv print-info out))


(close-output-port out)