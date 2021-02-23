#lang racket/gui

(require racket/runtime-path)

(provide tex-bitmap
         tex-size
         tex-section)

(define-runtime-path bitmap-path "./res/sprites.png")

;; Spritesheet loaded as a bitmap%.
(define tex-bitmap
  (read-bitmap bitmap-path 'png/alpha))

;; Size of texture in application.
(define tex-size 16)

;; Positions of textures on spritesheet. 
(define sprite-hash
  #hash((0 . (1 . 0))
        (1 . (0 . 1))
        (2 . (1 . 1))
        (3 . (2 . 1))
        (4 . (3 . 1))
        (5 . (4 . 1))
        (6 . (5 . 1))
        (7 . (6 . 1))
        (8 . (7 . 1))
        (hidden . (0 . 0))
        (flag . (2 . 0))
        (mine . (5 . 0))))

;; Returns the upper-left point of a value
;; on the spritesheet.
(define (tex-section value)
  (let ([pos (hash-ref sprite-hash value)])
    (cons (* (car pos) 17)
          (* (cdr pos) 17))))


    (define (generate-background! size)
      (let* ([result (make-object bitmap%
                       (* tex-size (car size))
                       (* tex-size (cdr size)))]
             [dc (new bitmap-dc%
                      [bitmap result])]
             [pos (tex-section 'hidden)])
        (for ([y (in-range (cdr size))])
          (for ([x (in-range (car size))])
            (send dc draw-bitmap-section tex-bitmap
                  (* x tex-size) (* y tex-size)
                  (car pos) (cdr pos)
                  tex-size tex-size)))
        result))