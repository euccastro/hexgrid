; Visualizer and demo for the hexgrid module
; ==========================================
;
; Usage:
;
;  - Move the mouse around to highlight hexagons.  This exercises
;    world->grid.
;
;  - Hexagons within the grid are highlighted yellow, hexagons outside the
;    grid are highlighted in cyan.  This exercises within-bounds?
;
;  - Move the white highlight around with the following keys, arranged in an
;    hexagon pattern in qwerty keyboards:
;
;                     Y  U
;                   G      J
;                     B  N
;
;    This exercises the direction functions (east, northeast, ...).
;
;  - When you do the above, the distances between the yellow/cyan hexagon and
;    the white one, both with and without taking wrap around into account,
;    will be printed out to the console.  This exercises the distance function.
;    For convenience, you can move the white highlight by clicking on an
;    hexagon.
;
;  - See how the code below picks a radius to fit the grid in the screen, and
;    an origin (center of the 0,0 hexagon) to center the grid in the screen.
;
;  - See also how to save locally curried versions of some of the functions,
;    to reduce clutter and verbosity.
;
;  - You may notice some lag while dragging the mouse.  This is caused by how
;    I poll for events in SDL, not by inefficiencies in the hexgrid module.

(load "hexgrid-module.scm")
(import hexgrid)

(use sdl-base gl srfi-42 bindings)

(define window-width 800)
(define window-height 600)
(define gwidth 9)
(define gheight 8)
(define gsize (list gwidth gheight))

; Calculate largest size that will fit in the screen (with `margin`).
(define hex-radius
  (bind (world-w1 world-h1) (world-size gsize 1)
    (let* ((margin (quotient (min window-width window-height) 10))
           (canvas-w (- window-width (* 2 margin)))
           (canvas-h (- window-height (* 2 margin))))
      (min (/ canvas-w world-w1) (/ canvas-h world-h1)))))

; Calculate origin display coordinates that will center the grid in the screen.
(define orig
  (bind (world-width world-height) (world-size gsize hex-radius)
    (list
      (+ (/ (- window-width world-width) 2)
         (* inner-radius hex-radius))
      (+ (/ (- window-height world-height) 2)
         hex-radius))))

; Make local curried versions of various functions sensitive to gsize, orig,
; and hex-radius.
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
(define selected '(0 0))

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
