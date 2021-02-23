#lang racket/gui

(require "GameGui.rkt")

(define main-gui%
  (class object%
    (field
     [main-frame (new frame%
                      [label "MS"]
                      [min-width 200]
                      [min-height 160]
                      [stretchable-width #f]
                      [stretchable-height #f])]
     [game (new game-gui%)])

    ;; ---- Private methods/definitions ----

    ;; Checks if a value is inside min/max boundaries.
    (define/private (get-value object)
      (string->number (send object get-value)))

    ;; Error checking an input.
    (define/private (legal-size? object min max)
      (let ([value (get-value object)])
        (and value
             (and (>= value min)
                  (<= value max)))))

    ;; Report invalid input back to user.
    (define/private (send-error! object min max)
      (send object set-value
            (string-append "Must be in "
                           (number->string min)
                           "<= x <="
                           (number->string max))))

    (define/private (on-play!)
      (let ([width (get-value width-input)]
            [height (get-value height-input)]
            [mines (get-value height-input)]
            [ai? (send ai-check get-value)])
        (void)))

    (define (button-procedure button control-event)
      (let ([width? (legal-size? width-input 3 30)]
            [height? (legal-size? height-input 3 16)])
        (cond
          ([not width?]
           (send-error! width-input 3 30))
          ([not height?]
           (send-error! height-input 3 16))
          (else
           (let* ([width (get-value width-input)]
                  [height (get-value height-input)]
                  [mine? (legal-size? mine-input
                                      1
                                      (* width height))])
             (cond
               ([not mine?]
                (send-error! mine-input 1 (* width height)))
               (else
                (on-play!))))))))

    ;; ---- Components for gui ----
    (define main-panel
      (new vertical-panel%
           [parent main-frame]
           [border 10]))

    (define width-input
      (new text-field%
           [label "Width "]
           [parent main-panel]
           [init-value "9"]))

    (define height-input
      (new text-field%
           [label "Height"]
           [parent main-panel]
           [init-value "9"]))

    (define mine-input
      (new text-field%
           [label "Mines "]
           [parent main-panel]
           [init-value "10"]))

    (define ai-check
      (new check-box%
           [label "AI"]
           [parent main-panel]))

    (define play-button
      (new button%
           [label "Play"]
           [parent main-panel]
           [callback button-procedure]))

    (send main-frame show #t)

    (super-new)))
