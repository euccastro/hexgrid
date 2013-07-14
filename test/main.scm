;XXX: find ways in which the hex API could be improved.

(load "hex.scm")

(use sdl-base gl srfi-42)

(define window-width 800)
(define window-height 600)

(sdl-init SDL_INIT_VIDEO)
(sdl-gl-set-attribute SDL_GL_RED_SIZE 8)
(sdl-gl-set-attribute SDL_GL_GREEN_SIZE 8)
(sdl-gl-set-attribute SDL_GL_BLUE_SIZE 8)
(sdl-gl-set-attribute SDL_GL_ALPHA_SIZE 8)
(sdl-gl-set-attribute SDL_GL_DEPTH_SIZE 16)
(sdl-gl-set-attribute SDL_GL_DOUBLEBUFFER 1)

(sdl-set-video-mode window-width window-height 32 SDL_OPENGL)

(gl:Viewport 0 0 window-width window-height)
(gl:MatrixMode gl:PROJECTION)
(gl:LoadIdentity)
(gl:Ortho 0 window-width 0 window-height -1 100)
(gl:MatrixMode gl:MODELVIEW)
(gl:ClearColor 0.5 0.5 0.5 0.0)
(gl:PointSize 4)

(define gwidth 9)
(define gheight 6)
(define gsize (list gwidth gheight))
(define hex-radius
  (bind (world-w1 world-h1) (world-size gsize 1)
    (let* ((margin (quotient (min window-width window-height) 10))
           (canvas-w (- window-width (* 2 margin)))
           (canvas-h (- window-height (* 2 margin))))
      (min (/ canvas-w world-w1) (/ canvas-h world-h1)))))
(define wsize (world-size gsize hex-radius))
(define world-width (car wsize))
(define world-height (cadr wsize))
(define orig
  (list
    (+ (/ (- window-width world-width) 2)
       (* inner-radius hex-radius))
    (+ (/ (- window-height world-height) 2)
       hex-radius)))

; Make local curried versions.
(define wrap (compose (horizontal-wrapper gwidth) (vertical-wrapper gheight)))
(define within-bounds? (within-bounds? gsize))
(define distance (distance gsize))
(define grid->world (grid->world orig hex-radius))
(define world->grid (world->grid orig hex-radius))

(define (draw-grid)
  (gl:Color3f 1.0 1.0 1.0)
  (do-ec (: i gwidth)
         (: j gheight)
         (draw-hex gl:LINE_LOOP (list i j)))
  (when mouseover
    (apply gl:Color3f
           (if (within-bounds? mouseover)
             '(1.0 1.0 0.0)
             '(0.0 1.0 1.0)))
    (draw-hex gl:POLYGON mouseover))
  (when selected
    (gl:Color3f 1.0 1.0 1.0)
    (draw-hex gl:POLYGON selected)))

(define (draw-hex gl-type cell)
  (gl:LoadIdentity)
  (bind (x y) (grid->world cell)
    (gl:Translatef x y 0))
  (gl:Scalef hex-radius hex-radius 0)
  (gl:Begin gl-type)
  (for-each
    (lambda (v)
      (apply gl:Vertex2f v))
    hex-verts)
  (gl:End))

(define ev (make-sdl-event))

(define done #f)
(define mouseover #f)
(define selected #f)

(define (cell-under-mouse sdl-ev)
  (world->grid
    (list
      (sdl-event-x ev)
      ; SDL has y downwards.
      (- window-height (sdl-event-y ev)))))

(define (print-distances)
  (when (and mouseover selected)
    (newline)
    (display "Distance without cut-around is: ")
    (display (distance-nowrap mouseover selected))
    (newline)
    (display "Distance with cut-around is: ")
    (display (distance mouseover selected))
    (newline)))

(let loop ()
  (gl:Clear gl:COLOR_BUFFER_BIT)
  (draw-grid)
  (sdl-gl-swap-buffers)
  (sdl-wait-event! ev)
  (let ((evt-type (sdl-event-type ev)))
    (when (eqv? evt-type SDL_MOUSEMOTION)
      (let ((new (cell-under-mouse ev)))
        (unless (equal? new mouseover)
          (set! mouseover new)
          (print-distances))))
    (when (eqv? evt-type SDL_MOUSEBUTTONDOWN)
      (set! selected (cell-under-mouse ev)))
    (when (and selected (eqv? evt-type SDL_KEYDOWN))
      (let ((sym (sdl-event-sym ev)))
        (for-each
          (lambda (k k2 direction)
            (when (or (eqv? sym k) (eqv? sym k2))
              (set! selected (wrap (direction selected)))
              (print-distances)))
          (list SDLK_j SDLK_u SDLK_y SDLK_g SDLK_b SDLK_n)
          (list SDLK_s SDLK_l SDLK_r SDLK_t SDLK_w SDLK_v)
          directions)))
    (when (not (or (eqv? evt-type SDL_QUIT)
                   (and (eqv? evt-type SDL_KEYDOWN)
                        (eqv? (sdl-event-sym ev) SDLK_ESCAPE))))
      (loop))))

(exit)
