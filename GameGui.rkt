#lang racket/gui

(require "Grid.rkt"
         "Utilities.rkt"
         "Solver.rkt")

(provide game-gui%)

(define game-gui%
  (class object%
    (field
     [game-frame (new frame%
                      [label "Gustav's MS"]
                      [min-width 1]
                      [min-height 1]
                      [stretchable-width #f]
                      [stretchable-height #f])]
     [grid (new grid%)]
     [solver (make-object solver% grid)]
     [background (make-object bitmap% 1 1)]
     [ai-active? #f]
     [run-game? #f])

    ;; Initialize the game and run it.
    (define/public (init-game! width height mines ai?)
      (init-size! width height)
      (set-mines! mines)
      (when ai?
        (send solver set-size! (cons width height))
        (set! ai-active? #t))
      (send game-frame center 'both)
      (send game-frame show #t)
      (start-game!))
    
    ;; ---- Private methods/definitions ----
    (define/private (generate-background! width height)
      (let* ([result (make-object bitmap%
                       (* tex-size width)
                       (* tex-size height))]
             [dc (new bitmap-dc%
                      [bitmap result])]
             [pos (tex-section 'hidden)])
        (for ([y (in-range height)])
          (for ([x (in-range width)])
            (send dc draw-bitmap-section tex-bitmap
                  (* x tex-size) (* y tex-size)
                  (car pos) (cdr pos)
                  tex-size tex-size)))
        (set! background result)))

    ;; Sets the size of the frame and the game.
    ;; Also generates the background.
    (define/private (init-size! width height)
      (send game-frame min-width (* tex-size
                                    width))
      (send game-frame min-height (* tex-size
                                     height))
      (send grid set-size! (cons width height))
      (generate-background! width height))

    ;; Set and generate mines on the grid.
    (define/private (set-mines! mines)
      (send grid set-mines! mines)
      (send grid generate-mines!))

    (define/private (start-game!)
      (send game-timer start 16 #f))

    ;; ---- Components for gui ----
    (define game-panel
      (new vertical-panel%
           [parent game-frame]))

    (define input-canvas%
      (class canvas%
        (init-field handler)

        (define/override (on-event event)
          (handler event))
        (super-new)))

    (define (mouse-handler mouse-event)
      (let ([x (floor (/ (send mouse-event get-x)
                         tex-size))]
            [y (floor (/ (send mouse-event get-y)
                         tex-size))])
        (cond
          ([send mouse-event button-up? 'left]
           (send grid flip-rec! (cons x y)))
          ([send mouse-event button-up? 'right]
           (send grid toggle-flag! (cons x y))))))

    ;; Paints the canvas each time it's called.
    (define (paint-procedure canvas dc)
      (send dc draw-bitmap background 0 0)
      (send grid draw-nodes dc))

    (define game-canvas
      (new input-canvas%
           [parent game-panel]
           [handler mouse-handler]
           [paint-callback paint-procedure]))

    ;; ---- Timers -----
    (define (update-game!)
      (send game-canvas refresh)
      (when ai-active?
        (send solver tick-solver!))
      (when (or (send grid lost?)
                (send grid win?))
        (send game-timer stop)))

    (define game-timer
      (new timer%
           [notify-callback update-game!]))

    (super-new)))