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
;
; License (BSD)
; -------------
;
; Copyright (C) 2013, Estevo U. C. Castro  <euccastro@gmail.com>
; All rights reserved.

; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:

; Redistributions of source code must retain the above copyright notice, this
; list of conditions and the following disclaimer.
; Redistributions in binary form must reproduce the above copyright notice,
; this list of conditions and the following disclaimer in the documentation
; and/or other materials provided with the distribution.
; Neither the name of the author nor the names of its contributors may be
; used to endorse or promote products derived from this software without
; specific prior written permission.

; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE
; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.

(use hexgrid)
(use sdl-base gl srfi-42 bindings)

(define window-width 800)
(define window-height 600)
(define window-size (list window-width window-height))

(define gwidth 9)
(define gheight 8)
(define gsize (list gwidth gheight))

(define hex-radius (radius-to-fit gsize window-size 20))

(define origin (origin-to-center gsize hex-radius window-size))

; Make local curried versions of various functions sensitive to gsize, orig,
; and hex-radius.
(define wrap (compose (horizontal-wrapper gwidth) (vertical-wrapper gheight)))
(define in-bounds? (within-bounds? gsize))
(define dist (distance gsize))
(define g->w (grid->world origin hex-radius))
(define w->g (world->grid origin hex-radius))

(define (draw-grid)
  (gl:Color3f 1.0 1.0 1.0)
  (do-ec (: i gwidth)
         (: j gheight)
         ; Draw every other row with a different method, to show they're
         ; equivalent.
         ((if (even? j)
            draw-hex-with-opengl-transform
            draw-hex-with-hexgrid-transform)
          gl:LINE_LOOP (list i j)))
  (when mouseover
    (apply gl:Color3f
           (if (in-bounds? mouseover)
             '(1.0 1.0 0.0)
             '(0.0 1.0 1.0)))
    (draw-hex-with-opengl-transform gl:POLYGON mouseover))
  (when selected
    (gl:Color3f 1.0 1.0 1.0)
    (draw-hex-with-hexgrid-transform gl:POLYGON selected)))

(define (draw-hex-with-opengl-transform gl-type cell)
  (gl:LoadIdentity)
  (bind (x y) (g->w cell)
    (gl:Translatef x y 0))
  (gl:Scalef hex-radius hex-radius 0)
  (draw-verts gl-type normalized-hex-verts))

(define hv (hex-verts origin hex-radius))
(define (draw-hex-with-hexgrid-transform gl-type cell)
  (gl:LoadIdentity)
  (draw-verts gl-type (hv cell)))

(define (draw-verts gl-type verts)
  (gl:Begin gl-type)
  (for-each
    (lambda (v)
      (apply gl:Vertex2f v))
    verts)
  (gl:End))

(define ev (make-sdl-event))

(define done #f)
(define mouseover #f)
(define selected '(0 0))

(define (cell-under-mouse sdl-ev)
  (w->g
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
    (display (dist mouseover selected))
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
