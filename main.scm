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

; XXX: factor out into utilities to fit grids to rectangles and viceversa.
(define hex-radius 50)
(define orig (ceiling (* hex-radius 1.2)))
(define ir (* hex-radius inner-radius))
(define hr (/ hex-radius 2))
(define gwidth (inexact->exact
                (floor (/ (- window-width orig)
                          (* ir 2)))))
(define gheight (* 2 (quotient (inexact->exact
                                 (floor (/ (- window-height orig)
                                           (* hr 3))))
                               2)))
(define (draw-grid width height)
  (gl:Color3f 1.0 1.0 1.0)
  (do-ec (: i width)
         (: j height)
         (draw-hex gl:LINE_LOOP i j))
  (when mouseover
    (apply gl:Color3f
           (if (apply within-bounds? width height mouseover)
             '(1.0 1.0 0.0)
             '(0.0 1.0 1.0)))
    (apply draw-hex gl:POLYGON mouseover))
  (when selected
    (gl:Color3f 1.0 1.0 1.0)
    (apply draw-hex gl:POLYGON selected)))

; XXX: utility to obtain transformed vertices directly?
(define (draw-hex gl-type i j)
  (gl:LoadIdentity)
  (receive (x y) (grid->world orig orig hex-radius i j)
    (gl:Translatef x y 0))
  (gl:Scalef hex-radius hex-radius 0)
  (gl:Begin gl-type)
  (for-each
    (lambda (v)
      (gl:Vertex2f (car v) (cadr v)))
    hex-verts)
  (gl:End))

(define ev (make-sdl-event))

(define done #f)
(define mouseover #f)
(define selected #f)

(define (cell-under-mouse sdl-ev)
  (receive
    (world->grid orig orig hex-radius
                 (sdl-event-x ev) (- window-height (sdl-event-y ev)))))

(let loop ()
  (gl:Clear gl:COLOR_BUFFER_BIT)
  (draw-grid gwidth gheight)
  (sdl-gl-swap-buffers)
  (sdl-wait-event! ev)
  (let ((evt-type (sdl-event-type ev)))
    (when (eqv? evt-type SDL_MOUSEMOTION)
      (let ((new (cell-under-mouse ev)))
        (unless (equal? new mouseover)
          (set! mouseover new)
          (when selected
            (display "Distance without cut-around is: ")
            (display (apply distance (append mouseover selected)))
            (newline)
            (display "Distance with cut-around is: ")
            (display (apply distance
                            (append mouseover selected
                                    (list grid-width: gwidth
                                          grid-height: gheight))))
            (newline)))))
    (when (eqv? evt-type SDL_MOUSEBUTTONDOWN)
      (set! selected (cell-under-mouse ev)))
    (when (and selected (eqv? evt-type SDL_KEYDOWN))
      (let ((sym (sdl-event-sym ev)))
        (for-each
          (lambda (k k2 direction)
            (when (or (eqv? sym k) (eqv? sym k2))
              (set! selected
                (receive (i j) (apply direction selected)
                  (receive (wrap gwidth gheight i j))))))
          (list SDLK_j SDLK_u SDLK_y SDLK_g SDLK_b SDLK_n)
          (list SDLK_s SDLK_l SDLK_r SDLK_t SDLK_w SDLK_v)
          directions)))
    (when (not (or (eqv? evt-type SDL_QUIT)
                   (and (eqv? evt-type SDL_KEYDOWN)
                        (eqv? (sdl-event-sym ev) SDLK_ESCAPE))))
      (loop))))

(exit)
