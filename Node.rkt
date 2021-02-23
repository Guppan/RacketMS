#lang racket/base

(provide node%)

(require racket/class)

(define node%
  (class object%
    (field [visible #f]
           [flag #f]
           [mine #f]
           [value 0])

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
      (set! visible #t))
    (define/public (toggle-flag!)
      (set! flag (not flag)))
    (define/public (set-mine!)
      (set! mine #t))
    (define/public (increment-value!)
      (set! value (add1 value)))

    (super-new)))