#lang racket

(require 2htdp/image)

;; A Posn is (posn number number)
(struct posn (x y) #:transparent)
;; Represents a two dimensional point.

;; A Goo is a (goo Posn Number)
(struct goo (loc expire) #:transparent)
;; The expire field is a Natural Number that represents the number
;; of ticks until the goo expires. A goo is expired when this field is 1

;; Board Size Constants
(define SIZE 30)

;; Snake Constants
(define SEG-SIZE 15)

;; GRAPHICAL BOARD
(define WIDTH-PX  (* SEG-SIZE 30))
(define HEIGHT-PX (* SEG-SIZE 30))

;; Visual constants
(define GOO-IMG (bitmap "graphics/goo.gif"))
(define SEG-IMG  (bitmap "graphics/body.gif"))
(define HEAD-IMG (bitmap "graphics/head.gif"))

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))




;; ACTUAL NEW STUFF: ----------------------------------------------

(define snake%
  (class object%
    (super-new)
    (init-field dir head tail)
    (inspect #f)

    (define/public (slither)
      (set! tail (cons head (all-but-last tail)))
      (set! head (next-head)))

    (define/public (grow)
      (set! tail (cons head tail))
      (set! head (next-head)))

    (define/public (can-eat goos)
      (findf (lambda (goo) (close? goo head)) goos))

    (define/private (next-head)
      (cond
        [(string=? dir "up")    (posn-move head 0 -1)]
        [(string=? dir "down")  (posn-move head 0 +1)]
        [(string=? dir "left")  (posn-move head -1 0)]
        [(string=? dir "right") (posn-move head +1 0)]))))


(define snake1
  (make-object snake% "left" (posn 1 2) '()))

(define snake2
  (new snake% [head (posn 3 9)] [tail '()] [dir "up"]))

(define snake-render%
  (class snake%
    (super-new)
    (inherit-field dir head tail)
    ;(inspect #f)    ;; make transparent

    (init-field [bgrd MT-SCENE] [img-db "image-file"])

    (define/public (render)
      (define head-img (select-head dir img-db))
      (define tail-img (img-list+scene tail SEG-IMG bgrd))
      (img+scene head head-img tail-img)) ))


(define (select-head a b)
  HEAD-IMG)


(define snake3
  (new snake-render% [dir "down"] [head (posn 4 3)] [tail '()]))

(define snake4
  (new snake-render% [dir "down"] [head (posn 4 3)] [tail '()]
       [img-db "new-image-file"]))


;; ----------------------------------------------

(define (today-is-tuesday?)
  (< 2 (random 7)))

(define snake5
  (new (if (today-is-tuesday?) snake% snake-render%)
       [dir "down"] [head (posn 3 4)] [tail '()]))

(define some-snake%
  (class (if (today-is-tuesday?) snake% snake-render%)
    (super-new)))



;; ----------------------------------------------

(define (add-render %)
  (class %
    (super-new)
    (inherit-field dir head tail)
    (init-field [bgrd MT-SCENE])

    (define/public (render)
      (define head-img HEAD-UP-IMG)
      (define tail-img (img-list+scene tail SEG-IMG bgrd))
      (img+scene head head-img tail-img))
    ))

(define snake-render-again%
  (add-render snake%))

(define snake6
  (new snake-render-again% [dir "right"] [head (posn 4 5)] [tail '()]))


;; ----------------------------------------------


;; Posn Number Number -> Posn
;; Move the position by dx, dy.
;; > (posn-move (posn 1 1) 2 3)
;; (posn 3 4)
(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

;; (cons X [Listof X]) -> [Listof X]
;; Returns a List that is does not contain the last element of the given list.
;; > (all-but-last '(1 2 3 4))
;; '(1 2 3)
(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) 
                    (all-but-last (rest segs)))]))

;; Seg Goo -> Boolean
;; Is the segment close to the goo?
;; > (close? (posn 1 2) (goo (posn 1 2) 4))
;; #t
(define (close? s g)
  (posn=? s (goo-loc g)))


;; Posn Posn -> Boolean
;; Are the two posns are equal?
;; > (posn=? (posn 1 1) (posn 1 1))
;; true
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

;; [Listof Posn] Image Scene -> Scene
;; Draws a the image to each posn in the list
;; > (img-list+scene (list (posn 1 1)) GOO-IMG MT-SCENE)
;; (place-image GOO-IMG 8 8
;;              (img-list+scene empty GOO-IMG MT-SCENE))
(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene (first posns)
                         img 
                         (img-list+scene (rest posns) img scene))]))

;; Posn Image Scene -> Scene
;; Draws a the given image onto the scene at the posn.
;; > (img+scene (posn 2 2) GOO-IMG MT-SCENE)
;; (place-image GOO-IMG 32 32 MT-SCENE)
(define (img+scene posn img scene)
  (place-image img 
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))

;                     
