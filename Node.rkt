#lang racket/base

(require "Utilities.rkt"
         racket/class)

(provide node%)

(define node%
  (class object%
    (init-field pos)
    (field [visible #f]
           [flag #f]
           [mine #f]
           [value 0]
           [texture (cons 0 0)])

    ;; ---- Getters ----
    (define/public (get-visible)
      visible)
    (define/public (get-flag)
      flag)
    (define/public (get-mine)
      mine)
    (define/public (get-value)
      value)

    ;; ---- Setters ----
    (define/public (set-visible!)
      (set! visible #t)
      (if mine
          (set! texture (tex-section 'mine))
          (set! texture (tex-section value))))
    
    (define/public (toggle-flag!)
      (set! flag (not flag))
      (when flag
        (set! texture (tex-section 'flag))))
    
    (define/public (set-mine!)
      (set! mine #t))
    
    (define/public (increment-value!)
      (set! value (add1 value)))

    ;; ---- Public methods ----
    (define/public (draw dc)
      (send dc draw-bitmap-section tex-bitmap
            (* tex-size (car pos)) (* tex-size (cdr pos))
            (car texture) (cdr texture)
            tex-size tex-size))

    (super-new)))