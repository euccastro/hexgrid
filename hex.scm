; Hexagon torus
; =============
;
; This module defines utility functions to work with hexagonal grids, such as
; those used in some wargames.
;
; The main data structure defined here represents a two dimensional hexagon
; grid that wraps around in both directions (i.e. that has torus topology).
;
; The grid is stored as a flat array, where the 0,0 cell is at the lowest left
; corner and width-1,height-1 is at the upper right.  The grid represents
; a roughly rectangular region in world/screen space.  For example,
; (make <grid> 'width 3 'height 4) would return a representation of the
; following grid:
;
;     0,3   1,3   2,3
;
;  0,2   1,2   2,2
;
;     0,1   1,1   2,1
;
; (0,0)  1,0   2,0
;
; Where indices wrap around, so coords -1,4 in the above grid refer to the
; same cell as 2,0. Thus, the above grid also represents the following map
; (where an instance of the original grid has been highlighted):
;
;                         ...
;
;    ...   1,1   2,1   0,1   1,1   2,1   0,1   1,1   ...
;
; ...   1,0   2,0   0,0   1,0   2,0   0,0   1,0   ...
;                     +-----------------+
;    ...   1,3   2,3 / 0,3   1,3   2,3 / 0,3   1,3   ...
;                   /                /
; ...   1,2   2,2 | 0,2   1,2   2,2 |  0,2   1,2   ...
;                  \                 \
;    ...   1,1   2,1 | 0,1   1,1   2,1 | 0,1   1,1   ...
;                  /                  /
; ...   1,0   2,0 /(0,0)  1,0   2,0 / 0,0   1,0   ...
;                +-----------------+
;    ...   1,3   2,3   0,3   1,3   2,3   0,3   1,3   ...
;
; ...   1,2   2,2   0,2   1,2   2,2   0,2   1,2   ...
;
;                        ...
;
; If this wrapping doesn't make sense for your application, just check indices
; at the appropriate places with (within-bounds grid i j).
;
; Warning: unless you initialize the grid for strict checking, height must be
; even.  Wrapping around wouldn't make sense otherwise.

; Internals
; =========
;
; Vector indices
; --------------
;
; We store grid cells in a flat vector, where the order is determined by
; cell coordinates, first x and then y, both ascending.  Thus, the grid
; described above would be represented as
;
;   #(0,0  1,0  2,0  3,0  0,1  1,1  2,1  3,1  0,2  1,2  2,2  3,2)


(use srfi-78 bindings)

(define (make-grid-vector width height)
  (assert (positive-integers? width height))
  (make-vector (* width height)))

(define (wrap grid-width grid-height i j)
  (values (modulo i grid-width)
          (modulo j grid-height)))

(define (index grid-width grid-height i j #!optional wrap-around)
  (when (and (not wrap-around)
             (not (within-bounds? grid-width grid-height i j)))
    (abort "Out of bounds"))
  (receive (i j) (if wrap-around
                   (wrap grid-width grid-height i j)
                   (values i j))
    (+ i (* j grid-width))))

; Copied for reference.
;    ...   1,1   2,1   0,1   1,1   2,1   0,1   1,1   ...
; ...   1,0   2,0   0,0   1,0   2,0   0,0   1,0   ...
;    ...   1,3   2,3   0,3   1,3   2,3   0,3   1,3   ...
; ...   1,2   2,2   0,2   1,2   2,2   0,2   1,2   ...
;    ...   1,1   2,1   0,1   1,1   2,1   0,1   1,1   ...
; ...   1,0   2,0  (0,0)  1,0   2,0   0,0   1,0   ...
;    ...   1,3   2,3   0,3   1,3   2,3   0,3   1,3   ...
; ...   1,2   2,2   0,2   1,2   2,2   0,2   1,2   ...

(define (east i j)
  (values (+ i 1) j))

(define (northeast i j)
  (values (+ i (diagonal-offset 'east j))
          (+ j 1)))

(define (northwest i j)
  (values (+ i (diagonal-offset 'west j))
          (+ j 1)))

(define (west i j)
  (values (- i 1) j))

(define (southwest i j)
  (values (+ i (diagonal-offset 'west j))
          (- j 1)))

(define (southeast i j)
  (values (+ i (diagonal-offset 'east j))
          (- j 1)))

(define (diagonal-offset h-direction j)
    (cond ((and (eqv? h-direction 'west) (even? j)) -1)
          ((and (eqv? h-direction 'east) (odd? j)) +1)
          (else 0)))

(define directions (list east northeast northwest west southwest southeast))

; Vertical distance between centers of hexagons in two adjacent rows, in outer
; radii.
(define row-height (/ 3 2))

; In outer radii.  This is also the horizontal distance between adjacent
; hexagons in adjacent rows, or half the distance between adjacent hexagons in
; the same row.
(define inner-radius (/ (sqrt 3) 2))

; Return the world coordinates of the center of the cell at i,j grid
; position, if cells have an outer radius of `radius` and the center of the
; cell at grid position 0,0 has world coordinates ox,oy.
(define (grid->world ox oy radius i j)
  (assert (> radius 0))
  (values (+ ox (* (+ (* 2 i) (if (odd? j) 1 0))
                   inner-radius
                   radius))
          (+ oy (* j row-height radius))))

; Return the (non-wrapped) grid coordinates of the cell that contains the
; world point (|x|,|y|), if cells have an outer radius of |radius| and the
; center of the cell at grid position (0,0) has world coordinates (|ox|,|oy|).
(define (world->grid ox oy radius x y)
  ; We begin by finding the position of the point in a rectangular grid where
  ; the center of every hexagon defines a new row and column.  The point is
  ; closest to the corners of the rectangle it is in than to any other hexagon
  ; center.  Each of these rectangles has two hexagon centers at opposite
  ; corners.  We find which of the corners with hexagon centers is closest,
  ; and that gives us the center of the hexagon where this point is.
  (let*-values
    (((rect-width rect-height i% j% x%% y%%)
      (world->rect% ox oy radius x y))
     ((x% y% di0 dj0 x1 y1 di1 dj1)
      ; What corners of the rectangle correspond to hex centers is arranged
      ; in a checkerboard pattern.
      (if (eq? (even? i%) (even? j%))
        ; We have translated our space so the lower left corner is at the
        ; center of the (0,0) hexagon, so all rectangles congruent to (0,0)
        ; checkerboard-wise will have hexagon centers in their lower left
        ; and upper right corners.
        (values 0 0 0 0
                rect-width rect-height 1 1)
        ; Thus, the others have hexagon centers in the lower right and upper
        ; left corners.
        (values rect-width 0 1 0
                0 rect-height 0 1)))
     ; Check which one is closest and obtain index offset.
     ((di dj) (if (< (squared-distance x%% y%% x% y%)
                     (squared-distance x%% y%% x1 y1))
                (values di0 dj0)
                (values di1 dj1))))
    (let ((j (+  dj j%)))
      ; Horizontally offset odd rows, divide by 2 because a rectangle is half
      ; an hexagon.
      (values (quotient (+ i% di (if (odd? j) -1 0))
                        2)
              j))))

; Factored out for debugging.
(define (world->rect% ox oy radius x y)
  (let* ((rect-width (* inner-radius radius))
         (rect-height (* row-height radius))
         (x% (- x ox))
         (y% (- y oy))
         ; The lower left corner of the rectangle where this point is.
         (i% (inexact->exact (floor (/ x% rect-width))))
         (j% (inexact->exact (floor (/ y% rect-height))))
         ; The position of the point relative to col and row.  We'll use this
         ; to find the closest hexagon center and thus obtain an offset from
         ; col and row.
         (x%% (- x% (* rect-width i%)))
         (y%% (- y% (* rect-height j%))))
    (values rect-width rect-height i% j% x%% y%%)))

; In counter-clockwise order, assuming center at 0,0 and radius 1.
(define hex-verts
  (let ((ir inner-radius))
    `((,ir -1/2) (,ir 1/2) (0 1) (,(- ir) 1/2) (,(- ir) -1/2) (0 -1))))

(define (within-bounds? grid-width grid-height i j)
  (assert (positive-integers? grid-width grid-height))
  (and (<= 0 i) (< i grid-width)
       (<= 0 j) (< j grid-height)))

(define distance
  (lambda (i0 j0 i1 j1 #!key grid-width grid-height)
    (let* ((i0 (+ i0 (if (odd? j0) 0.5 0)))
           (i1 (+ i1 (if (odd? j1) 0.5 0)))
           (nwdi (abs (- i1 i0)))
           (nwdj (abs (- j1 j0)))
           (di (if grid-width
                 (min nwdi (- grid-width nwdi))
                 nwdi))
           (dj (if grid-height
                 (min nwdj (- grid-height nwdj))
                 nwdj)))
      (+ dj (max 0 (inexact->exact
                     (- (ceiling di) (ceiling (/ dj 2)))))))))

(define (world-size grid-width grid-height radius)
  (values (* (+ (* grid-width 2)
                ; Offset for odd rows, if any.
                (if (> grid-height 1) 1 0))
             inner-radius radius)
          (* (+ 1/2 ; bottom half of first row.
                (* row-height grid-height))
             radius)))

; Tests.

(define (run-tests)

  (define grid-width 3)
  (define grid-height 4)

  ; Copied for reference.
  ;    ...   1,1   2,1   0,1   1,1   2,1   0,1   1,1   ...
  ; ...   1,0   2,0   0,0   1,0   2,0   0,0   1,0   ...
  ;    ...   1,3   2,3   0,3   1,3   2,3   0,3   1,3   ...
  ; ...   1,2   2,2   0,2   1,2   2,2   0,2   1,2   ...
  ;    ...   1,1   2,1   0,1   1,1   2,1   0,1   1,1   ...
  ; ...   1,0   2,0  (0,0)  1,0   2,0   0,0   1,0   ...
  ;    ...   1,3   2,3   0,3   1,3   2,3   0,3   1,3   ...
  ; ...   1,2   2,2   0,2   1,2   2,2   0,2   1,2   ...

  ; Move in all directions.
  (test-values "east even" 1 0 east 0 0)
  (test-values "northeast even" 0 1 northeast 0 0)
  (test-values "northwest even" -1 1 northwest 0 0)
  (test-values "west even" -1 0 west 0 0)
  (test-values "southwest even" -1 -1 southwest 0 0)
  (test-values "southeast even" 0 -1 southeast 0 0)
  (test-values "east odd" 1 1 east 0 1)
  (test-values "northeast odd" 1 2 northeast 0 1)
  (test-values "northwest odd" 0 2 northwest 0 1)
  (test-values "west odd" -1 1 west 0 1)
  (test-values "southwest odd" 0 0 southwest 0 1)
  (test-values "southeast odd" 1 0 southeast 0 1)

  ; Set and read.
  (let ((vec (make-grid-vector grid-width grid-height)))
    (vector-set! vec (index grid-width grid-height -1 -1 #t) 'hello)
    (check (vector-ref vec (index grid-width grid-height -1 -1 #t)) => 'hello)
    (check (vector-ref vec (index grid-width grid-height 2 3)) => 'hello))

  ; grid->world
  (test-values "grid->world minimal" 0.0 0.0 grid->world 0 0 1 0 0)
  (test-values "grid->world move origin" 3.0 4.0 grid->world 3 4 1 0 0)
  (test-values "grid->world move x"
               (* inner-radius 2) 0.0
               grid->world 0 0 1 1 0)
  (test-values "grid->world move y (odd)"
               inner-radius row-height
               grid->world 0 0 1 0 1)
  (test-values "grid->world negative x"
               (* inner-radius -4) 0.0
               grid->world 0 0 1 -2 0)
  (test-values "grid->world negative y"
               (* inner-radius -2) (* row-height -2)
               grid->world 0 0 1 -1 -2)

  ; world->gridbindings module
  (test-values "world->grid basic" 0 0 world->grid 0 0 1 0 0)
  (test-values "world->grid move x" 1 0 world->grid 0 0 1 1 0)
  (test-values "world->grid move origin x" -1 0 world->grid 1 0 1 0 0)
  (test-values "world->grid move origin y" 0 -1 world->grid 0.0 1.2 1 0.2 0)
  (let ((sectors '(("a" 0.5 0.2) ("b" 0.0 0.5) ("c" -0.5 0.2)
                                 ("d" -0.5 -0.2) ("e" 0.0 -0.5) ("f" 0.5 -0.2)))
        (cells `(("even" 0 0 0 0) ("odd" 0 1 ,inner-radius ,row-height))))
    (for-each
      (lambda (sector)
        (for-each
          (lambda (cell)
            (bind (name expi expj offsetx offsety) cell
                  (bind (sector-name sectorx sectory) sector
                        (test-values (string-append name "-" sector-name)
                                     expi expj world->grid 0 0 1
                                     (+ offsetx sectorx) (+ offsety sectory)))))
          cells))
      sectors))

  ; Distance (no wrap-around shortcuts).
  (check (distance 0 0 0 0) => 0)
  (check (distance 0 0 1 0) => 1)
  (check (distance 0 0 2 0) => 2)
  (check (distance 0 0 0 1) => 1)
  (check (distance 0 0 1 1) => 2)
  (check (distance 0 0 2 1) => 3)
  (check (distance 0 0 0 2) => 2)
  (check (distance 0 0 1 2) => 2)
  (check (distance 0 0 2 2) => 3)
  (check (distance 0 0 0 3) => 3)
  (check (distance 0 0 1 3) => 3)
  (check (distance 0 0 2 3) => 4)
  ; You can check distance to out-of-bounds coordinates without providing
  ; width and height.  They will be calculated as though those coordinates
  ; were within the grid, but without cutting around.  So distance to -1,-1 is
  ; smaller than to 2,3 even though they would be the same hex in
  ; a wrap-around grid.
  (check (distance 0 0 -1 -1) => 1)

  ; Distance considering wrap-around shourtcuts.
  (for-each
    (lambda (l)
      (bind (i j d) l
            (check (distance 0 0 i j
                             grid-width: grid-width grid-height: grid-height)
                   => d)))
    '((0 0 0)
      (1 0 1) (-1 0 1) (0 1 1) (-1 1 1) (0 -1 1) (-1 -1 1) (2 3 1)
      (1 1 2) (0 2 2) (1 2 2) (2 2 2) (1 3 2)))

  (test-values "world-size trivial" (* 2 inner-radius) 2.0
               world-size 1 1 1)
  (test-values "world-size" (* 21 inner-radius) 19.5
               world-size grid-width grid-height 3)
  (check-report))

; Utility functions.

(define (squared-distance x0 y0 x1 y1)
  (+ (square (- x1 x0)) (square (- y1 y0))))

(define (square x) (* x x))

(define (positive-integers? . l)
  (every (lambda (x) (and integer? x) (> x 0)) l))

(define (test-values s expa expb f . rest)
  (newline)
  (display s)
  (receive (a b) (apply f rest)
    (check a => expa)
    (check b => expb)))

(run-tests)
