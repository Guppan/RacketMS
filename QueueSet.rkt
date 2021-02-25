#lang racket/base

(require racket/class
         racket/set)

(provide queue-set%)

(define queue-set%
  (class object%
    (field [front-set (mutable-set)]
           [back-set (mutable-set)])

    (define/public (print port)
      (display "Front : " port)
      (for ([f (in-set front-set)])
        (display f port)
        (display " " port))
      (displayln "" port)
      (display "Back : " port)
      (for ([b (in-set back-set)])
        (display b port)
        (display " " port))
      (displayln "" port))

    ;; ---- Getters ----
    (define/public (get-front-set)
      front-set)
    (define/public (get-back-set)
      back-set)

    ;; ---- Public methods ----
    (define/public (contains? pos)
      (or (set-member? front-set pos)
          (set-member? back-set pos)))

    (define/public (move-back!)
      (set-union! back-set front-set)
      (set-clear! front-set))

    (define/public (add-set! set-arg move-back?)
      (when move-back?
        (set-union! back-set front-set))
      (set! front-set set-arg))

    (define/public (add-pos! pos move-back?)
      (when move-back?
        (move-back!))
      (set-add! front-set pos))
    
    (super-new)))