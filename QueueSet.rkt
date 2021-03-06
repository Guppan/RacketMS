#lang racket/base

(require racket/class
         racket/set)

(provide queue-set%)

(define queue-set%
  (class object%
    (field [front-set (mutable-set)]
           [back-set (mutable-set)])

    ;; ---- Getters ----
    (define/public (get-front-set)
      front-set)
    (define/public (get-back-set)
      back-set)

    ;; ---- Public methods ----
    (define/public (move-back!)
      (set-union! back-set front-set)
      (set-clear! front-set))

    (define/public (add-set! set-arg)
      (set-union! back-set front-set)
      (set! front-set set-arg))

    (define/public (add-pos! pos)
      (move-back!)
      (set-add! front-set pos))
    
    (super-new)))