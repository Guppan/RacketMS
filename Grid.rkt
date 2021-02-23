#lang racket/base

(require "Node.rkt"
         racket/class)

(define grid%
  (class object%
    (field [size (cons 0 0)]
           [mines 0]
           [grid (vector)]
           [lost #f]
           [flipped 0])

    ;; ---- Getters ----
    (define/public (lost?)
      lost)

    (define/public (win?)
      (= (+ mines flipped)
         (* (car size) (cdr size))))

    (define/public (get-value pos)
      (send (get-node pos) get-value))

    ;; ---- Setters ----
    (define/public (set-size! size-arg)
      (set! size size-arg)
      (set! grid (build-vector
                  (* (car size) (cdr size))
                  (lambda (unused)
                    (new node%)))))

    (define/public (set-mines! mine-arg)
      (set! mines mine-arg))

    ;; ---- Public methods ----
    (define/public (generate-mines!)
      (generate-mines-rec mines))

    (define/public (toggle-flag! pos)
      (let ([node (get-node pos)])
        (unless (send node get-visible)
          (send node toggle-flag!))))

    ;; Flips a single position.
    (define/public (flip-single! pos)
      (let* ([node (get-node pos)]
             [visible? (send node get-visible)]
             [flag? (send node get-flag)]
             [mine? (send node get-mine)])
        (unless (or visible? flag?)
          (send node set-visible)
          (set! flipped (add1 flipped))
          (when mine?
            (set! lost #t)))))

    ;; Recursively flip positions beginning with 'pos'.
    (define/public (flip-rec! pos)
      (let* ([node (get-node pos)]
             [visible? (send node get-visible)]
             [flag? (send node get-flag)])
        (unless (or visible? flag?)
          (let ([mine? (send node get-mine)]
                [value (send node get-value)])
            (send node set-visible)
            (set! flipped (add1 flipped))
            (cond
              (mine?
               (set! lost #t))
              (else
               (when (zero? value)
                 (for ([adjacent (adjacent-list pos)])
                   (flip-rec! adjacent)))))))))
    

    ;; ---- Private methods ----
    (define/private (in-range? pos)
      (and (and (>= (car pos) 0)
                (< (car pos) (car size)))
           (and (>= (cdr pos) 0)
                (< (cdr pos) (cdr size)))))

    (define/private (to-index pos)
      (+ (* (cdr pos) (car size))
         (car pos)))

    (define/private (get-node pos)
      (vector-ref grid (to-index pos)))
    
    ;; Returns a list of positions adjacent to
    ;; a given position 'pos'.
    (define/private (adjacent-list pos)
      (let ([result (list)])
        (for ([y (in-range (- (cdr pos) 1)
                           (+ (cdr pos) 2))]
              [x (in-range (- (car pos) 1)
                           (+ (car pos) 2))])
          (let ([new-pos (cons x y)])
            (when (and (in-range? new-pos)
                       (not (equal? new-pos pos)))
              (set! result (append result
                                   (list new-pos))))))
        result))

    ;; Increment the value for each adjacent node
    ;; to the node at position 'pos'.
    (define/private (increment-adjacent! pos)
      (for ([adjacent (adjacent-list pos)])
        (let ([node (get-node adjacent)])
          (send node increment-value!))))

    ;; Recursively generates mines.
    (define/private (generate-mines-rec remaining)
      (when (> remaining 0)
        (let* ([pos (cons (random (car size))
                          (random (cdr size)))]
               [node (get-node pos)])
          (cond
            ([send node get-mine]
             (generate-mines-rec remaining))
            (else
             (send node set-mine!)
             (increment-adjacent! pos)
             (generate-mines-rec (sub1 remaining)))))))
    
    (super-new)))