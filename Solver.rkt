#lang racket/base

(require "QueueSet.rkt"
         "Sentence.rkt"
         "Grid.rkt"
         racket/class
         racket/set)

(provide solver%)

(define solver%
  (class object%
    (init-field grid)
    (field [size (cons 0 0)]
           [made-moves (mutable-set)]
           [known-safes (new queue-set%)]
           [known-mines (new queue-set%)]
           [knowledge (make-hash)])

    (define/public (set-size! size-arg)
      (set! size size-arg))

    (define/public (tick-solver!)
      (let* ([pos (produce-move)]
             [count (send grid get-value pos)])
        (mark-add-sentence! pos count)
        (update-knowledge-loop!)
        (remove-filter-empty-sentence!)
        (conclude-new-knowledge!)
        (update-knowledge-loop!)
        (remove-filter-empty-sentence!)))

    ;; ---- Private methods ----

    ;; ---- mark-add-sentence! ----

    ;; Marks a position as a made move.
    ;; Complexity : constant
    (define/private (mark-move! pos)
      (send grid flip-single! pos)
      (set-add! made-moves pos))

    ;; Marks a position as safe and updates knowledge.
    ;; Complexity : knowledge.size
    (define/private (mark-safe! pos)
      (send known-safes add-pos! pos #t)
      (for ([sentence (in-hash-values knowledge)])
        (send sentence mark-safe! pos)))

    ;; Adds a sentence% to knowledge.
    ;; Complexity : constant
    (define/private (add-sentence! pos count)
      (hash-set! knowledge
                 pos
                 (make-object sentence% pos count size)))

    ;; Mark a 'pos' as a made move and a safe position then
    ;; add a sentence to knowledge based on 'pos'.
    ;; Complexity : knowledge.size
    (define/private (mark-add-sentence! pos count)
      (mark-move! pos)
      (mark-safe! pos)
      (add-sentence! pos count))

    ;; ---- Update-knowledge-loop! ----

    ;; Collects all sets from knowledge with 'get-method'
    ;; and adds the collection to 'known-set'.
    ;; Returns #t if new sets were collected, else #f.
    ;; Complexity : knowledge.size
    (define/private (update-known-set! known-set get-method)
      (let ([new-set (mutable-set)])
        (for ([sentence (in-hash-values knowledge)]
              #:unless (send sentence empty?))
          (set-union! new-set (dynamic-send sentence get-method)))
        (send known-set add-set! new-set #t)
        (not (set-empty? new-set))))

    (define/private (update-known-safes!)
      (update-known-set! known-safes 'get-safes))

    (define/private (update-known-mines!)
      (update-known-set! known-mines 'get-mines))

    ;; Marks new safes and mines in knowledge.
    ;; Complexity : knowledge.size * (known-safes.front-set.size +
    ;;                                known-mines.front-set.size)
    (define/private (mark-safes-mines!)
      (for ([sentence (in-hash-values knowledge)])
        (for ([safe-pos (in-set (send known-safes get-front-set))])
          (send sentence mark-safe! safe-pos))
        (for ([mine-pos (in-set (send known-mines get-front-set))])
          (send sentence mark-mine! mine-pos))))

    ;; Collects and marks new safes and mines as long
    ;; as possible.
    ;; Complexity : at least 2 * knowledge.size
    (define/private (update-knowledge-loop!)
      (when (or (update-known-safes!) (update-known-mines!))
        (mark-safes-mines!)
        (update-knowledge-loop!)))

    ;; ---- remove-filter-empty-sentence! ----
    
    ;; Returns a set of positions for sentence's which
    ;; are empty.
    ;; Complexity : knowledge.size
    (define/private (collect-empty-sentence)
      (let ([empty-set (mutable-set)])
        (for ([(pos sentence) (in-hash knowledge)]
              #:when (send sentence empty?))
          (set-add! empty-set pos))
        empty-set))

    ;; Removes all entries in knowledge with keys
    ;; in 'empty-set'.
    ;; Complexity : empty-set.size
    (define/private (remove-empty-sentence! empty-set)
      (for ([pos (in-set empty-set)])
        (hash-remove! knowledge pos)))

    ;; Removes all positions in 'empty-set' from
    ;; all adjacent-set's in each entry in knowledge.
    ;; Complexity : knowledge.size * empty-set.size
    (define/private (remove-empty-adjacent! empty-set)
      (for ([sentence (in-hash-values knowledge)])
        (send sentence remove-adjacent-set! empty-set)))

    ;; Removes all empty sentence's from knowledge
    ;; and updates all adjacent-sets in knowledge.
    ;; Complexity : knowledge.size * empty-set.size
    (define/private (remove-filter-empty-sentence!)
      (let ([empty-set (collect-empty-sentence)])
        (remove-empty-sentence! empty-set)
        (remove-empty-adjacent! empty-set)))

    ;; ---- conclude-new-knowledge! ----
    
    ;; Iterates knowledge and concludes new
    ;; sentences when possible using the subset-rule.
    ;; Complexity : at most 24 * knowledge.size
    (define/private (conclude-new-knowledge!)
      (for ([sentence (in-hash-values knowledge)])
        (for ([pos (in-set (send sentence get-adjacent))]
              #:when (hash-ref knowledge pos #f))
          (let ([other-sentence (hash-ref knowledge pos)])
            (send sentence conclude-sentence! other-sentence)))))

    ;; ---- produce-move ----
    
    ;; Returns a safe position or #f if no such
    ;; move is available.
    ;; Complexity : at most known-safes.back-set.size
    (define/private (make-safe-move)
      (for/first ([safe-pos (in-set (send known-safes get-back-set))]
                  #:unless (set-member? made-moves safe-pos))
        safe-pos))

    ;; Retrieve a key for knowledge for the sentence
    ;; which has the highest difference (sentence.count - count).
    ;; Complexity : knowledge.size
    (define/private (get-max-key)
      (let ([max -1]
            [key #f])
        (for ([(pos sentence) (in-hash knowledge)]
              #:when (> (send sentence get-difference) max))
          (set! max (send sentence get-difference))
          (set! key pos))
        key))

    ;; Returns a move from knowledge or #f
    ;; if no such move can be found.
    ;; Complexity : knowledge.size
    (define/private (make-knowledge-move)
      (let* ([key (get-max-key)]
             [sentence (hash-ref knowledge key #f)])
        (if sentence
            (set-first (send sentence get-sentence))
            #f)))

    ;; Produce a random move not known to be
    ;; a mine.
    (define/private (make-random-move)
      (let ([pos (cons (random (car size))
                       (random (cdr size)))])
        (if (or (set-member? made-moves pos)
                (set-member? (send known-mines get-back-set) pos))
            (make-random-move)
            pos)))

    ;; Produces a move.
    ;; Complexity : at least known-safes.back-set.size +
    ;;                       knowledge.size
    (define/private (produce-move)
      (let ([safe-move (make-safe-move)]
            [knowledge-move (make-knowledge-move)])
        (cond
          (safe-move safe-move)
          (knowledge-move knowledge-move)
          (else
           (make-random-move)))))
    
    (super-new)))