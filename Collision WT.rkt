
#lang racket

(require 2htdp/image
         (only-in mred make-bitmap bitmap-dc%)
         (only-in 2htdp/private/image-more render-image))

(provide collide?)

;; collide? Image Number Number Number Number -> Boolean
;; consumes: image is an image
;;           image-x is the x coordinate of where image will be drawn
;;           image-y is the y coordinate of where image will be drawn
;;           mouse-x is the x coordinate of the mouse
;;           mouse-y is the y coordinate of the mouse
;; produces: true if the mouse coordinates are over a non-transparent part of the image
;;           false otherwise
(define (collide? image image-x image-y mouse-x mouse-y)
  (local [(define m1 (image->mask image))
          (define w1 (image-width image))
          (define h1 (vector-length m1))
          (define dx (round (- mouse-x (- image-x (/ w1 2)))))
          (define dy (round (- mouse-y (- image-y (/ h1 2)))))]
    (for/or ((y (in-range (max 0 (- dy)) (min (- h1 dy) 1))))
            (not (zero? (bitwise-and (vector-ref m1 (+ y dy))
                                     (arithmetic-shift 1 dx)))))))

(define (image->mask image)
  (define w (image-width image))
  (define h (image-height image))
  (define bm (make-bitmap w h))
  (define bdc (make-object bitmap-dc% bm))
  (render-image image bdc 0 0)
  (for/vector ((y (in-range h)))
              (define alpha-bytes 
                (make-bytes (* 4 w)))
              (send bdc get-argb-pixels 0 y w 1 alpha-bytes #t)
              (for/sum ((x (in-range w)))
                       (if (zero? (bytes-ref alpha-bytes (* 4 x)))
                           0
                           (arithmetic-shift 1 x)))))

