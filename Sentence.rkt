#lang racket/base

(require racket/class
         racket/set)

(provide sentence%)

;; Represent a sentence on the form
;; {A, B, ..., N} = z
;; where {A, B, ..., N} is a set of positions
;; on the form (x, y) and z is the number
;; of mines adjacent to any of those positions.
(define sentence%
  (class object%
    (init-field pos
                count
                size)
    (field [sentence (create-set 1)]
           [adjacent (create-set 2)])

    (define/public (print port)
      (display pos port)
      (display " -> " port)
      (display "{" port)
      (for ([p (in-set sentence)])
        (display "(" port)
        (display (car p) port)
        (display "," port)
        (display (cdr p) port)
        (display ")" port))
      (display "}=" port)
      (display count port)
      (displayln "" port))

    (define/public (get-count)
      count)
    
    ;; ---- Getters ----
    (define/public (empty?)
      (set-empty? sentence))

    (define/public (get-difference)
      (- (set-count sentence) count))

    (define/public (get-sentence)
      sentence)

    (define/public (get-adjacent)
      adjacent)

    (define/public (get-safes)
      (if (zero? count)
          sentence
          (mutable-set)))

    (define/public (get-mines)
      (if (= count (set-count sentence))
          sentence
          (mutable-set)))
    
    ;; ---- Public methods ----
    (define/public (mark-safe! pos-arg)
      (when (set-member? sentence pos-arg)
        (set-remove! sentence pos-arg)))

    (define/public (mark-mine! pos-arg)
      (when (set-member? sentence pos-arg)
        (set-remove! sentence pos-arg)
        (set! count (sub1 count))))

    ;; Remove every position in 'a-set' from
    ;; 'adjacent'.
    ;; Complexity : a-set.size
    (define/public (remove-adjacent-set! a-set)
      (set-subtract! adjacent a-set))

    ;; Remove another sentence% from this adjacent-set.
    (define/public (remove-adjacent! other pass-on?)
      (set-remove! adjacent (get-field pos other))
      (when pass-on?
        (send other remove-adjacent! this #f)))

    ;; If another sentence is a subset to this sentence
    ;; we can conclude a new sentence.
    (define/public (conclude-sentence! other)
      (when (is-subset? other)
        (subtract-sentence! other)
        (subtract-count! other)
        (remove-adjacent! other #t)))
        

    ;; ---- Private methods ----
    (define/private (is-subset? other)
      (subset? (get-field sentence other)
               sentence))

    (define/private (subtract-sentence! other)
      (set-subtract! sentence
                     (get-field sentence other)))

    (define/private (subtract-count! other)
      (set! count (- count
                     (get-field count other))))
       
    (define/private (in-range? pos-arg)
      (and (and (>= (car pos-arg) 0)
                (< (car pos-arg) (car size)))
           (and (>= (cdr pos-arg) 0)
                (< (cdr pos-arg) (cdr size)))))

    ;; Creates a set of positions adjacent to 'pos'
    ;; in a radius 'limit'.
    (define/private (create-set limit)
      (let ([new-set (mutable-set)])
        (for ([y (in-range (- (cdr pos) limit)
                           (+ (cdr pos) (add1 limit)))])
          (for ([x (in-range (- (car pos) limit)
                             (+ (car pos) (add1 limit)))])
            (let ([new-pos (cons x y)])
              (when (and (in-range? new-pos)
                         (not (equal? new-pos pos)))
                (set-add! new-set new-pos)))))
        new-set))

    (super-new)))