#lang racket/gui

(require "Utilities.rkt")

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
     [size (cons 0 0)]
     [background (make-object bitmap% 1 1)])

    ;; Sets the frames size and generates
    ;; the background.
    (define/public (init-size! width height)
      (send game-frame min-width (* tex-size
                                    width))
      (send game-frame min-height (* tex-size
                                     height))
      (set! size (cons width height))
      (generate-background!))

    ;; ---- Private methods/definitions ----
    (define/private (generate-background!)
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
        (set! background result)))

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
           (void))
          ([send mouse-event button-up? 'right]
           (void)))))

    ;; Paints the canvas each time it's called.
    (define (paint-procedure canvas dc)
      (send dc draw-bitmap background 0 0))

    (define game-canvas
      (new input-canvas%
           [parent game-panel]
           [handler mouse-handler]
           [paint-callback paint-procedure]))

    ;; ---- Timers -----
    (define (update-game!)
      (send game-canvas refresh))

    (define game-timer
      (new timer%
           [notify-callback update-game!]))

    (super-new)))