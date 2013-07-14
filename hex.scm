; hexgrid
; =======
;
; This module defines utility functions to work with hexagonal grids, such as
; those used in some wargames.
;
; The grid is stored as a flat vector, where the 0,0 cell is at the lowest
; left corner and width-1,height-1 is at the upper right.  The grid represents
; a roughly rectangular region in world/screen space.  For example,
; (make-grid-vector '(3 4)) would return a representation of the following
; grid:
;
;     0,3   1,3   2,3
;
;  0,2   1,2   2,2
;
;     0,1   1,1   2,1
;
; (0,0)  1,0   2,0
;
; Where indices may optionally wrap around horizontally or vertically, so
; coords -1,4 in the above grid refer to the same cell as 2,0. Thus, the above
; grid also represents the following map (where an instance of the original
; grid has been highlighted):
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
; at the appropriate places with ((within-bounds? grid-size) cell).
;
; Warning: if you want your grid to wrap around vertically, make sure it has
; an even height (number of rows).
;
; Vector indices
; --------------
;
; We store grid cells in a flat vector, where the order is determined by
; cell coordinates, first x and then y, both ascending.  Thus, the grid
; described above would be represented as
;
;   #(0,0  1,0  2,0  3,0  0,1  1,1  2,1  3,1  0,2  1,2  2,2  3,2)
;
; Where i,j are the contents at cell in row j and column i.  These are
; basically whatever you set.  This module initializes them to #f and
; doesn't otherwise bother itself with them.
;
; Coordinate format
; -----------------
;
; Functions normally take and return sequences of size two to represent cell
; coordinates or points in world/screen space.  This is done so they compose
; more seamlessly.
;
; Currying
; --------
;
; Functions that take data that will typically not change much during a
; program come curried like this: ((fn permanent args) volatile args), to
; make it more convenient to avoid clutter and verbosity.

(use bindings)

; Internal helper to destructure 2-sequences.
(define-syntax def2
  (syntax-rules ()
    ((_ (f i j seq2) body ...)
     (define (f seq2)
         (bind (i j) seq2
           body ...)))))

(def2 (make-grid-vector width height size)
  (assert (positive-integers? width height))
  (make-vector (* width height) #f))

(def2 (indexer width height size)
  (assert (positive-integers? width height))
  (lambda (cell)
    (assert ((within-bounds? size) cell))
    (bind (i j) cell
      (+ i (* j width)))))

(define (horizontal-wrapper width)
  (assert (positive-integers? width))
  (lambda (cell)
    (bind (i j) cell
      (list (modulo i width) j))))

(define (vertical-wrapper height)
  (assert (and (positive-integers? height) (even? height)))
  (lambda (cell)
    (bind (i j) cell
      (list i (modulo j height)))))

; Copied for reference.
;    ...   1,1   2,1   0,1   1,1   2,1   0,1   1,1   ...
; ...   1,0   2,0   0,0   1,0   2,0   0,0   1,0   ...
;    ...   1,3   2,3   0,3   1,3   2,3   0,3   1,3   ...
; ...   1,2   2,2   0,2   1,2   2,2   0,2   1,2   ...
;    ...   1,1   2,1   0,1   1,1   2,1   0,1   1,1   ...
; ...   1,0   2,0  (0,0)  1,0   2,0   0,0   1,0   ...
;    ...   1,3   2,3   0,3   1,3   2,3   0,3   1,3   ...
; ...   1,2   2,2   0,2   1,2   2,2   0,2   1,2   ...

(def2 (east i j cell)
  (list (+ i 1) j))

(def2 (northeast i j cell)
  (list (+ i (diagonal-offset 'east j))
        (+ j 1)))

(def2 (northwest i j cell)
  (list (+ i (diagonal-offset 'west j))
          (+ j 1)))

(def2 (west i j cell)
  (list (- i 1) j))

(def2 (southwest i j cell)
  (list (+ i (diagonal-offset 'west j))
          (- j 1)))

(def2 (southeast i j cell)
  (list (+ i (diagonal-offset 'east j))
          (- j 1)))

; Utility.
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

; Return the world coordinates of the center of the `cell`, if cells have an
; outer radius of `radius` and the center of the cell at grid position 0,0 is
; at `origin` in world/screen coordinates.
(define ((grid->world origin radius) cell)
  (bind-let (((ox oy) origin) ((i j) cell))
    (assert (> radius 0))
    (list (+ ox (* (+ (* 2 i) (if (odd? j) 1 0))
                   inner-radius
                   radius))
          (+ oy (* j row-height radius)))))

; Return the (non-wrapped) grid coordinates of the cell that contains the
; `point`, if cells have an outer radius of `radius` and the center of the
; cell at grid position (0,0) is at `origin` in world/screen coordinates.
(define ((world->grid origin radius) point)
  ; We begin by finding the position of the point in a rectangular grid where
  ; the center of every hexagon defines a new row and column.  The point is
  ; closest to the corners of the rectangle it is in than to any other hexagon
  ; center.  Each of these rectangles has two hexagon centers at opposite
  ; corners.  The point is inside the hexagon with the closest center.
  (bind-let*
    (((ox oy) origin)
     ((x y) point)
     ((rect-width rect-height i% j% x%% y%%)
      (world->rect% ox oy radius x y))
     ((x% y% di0 dj0 x1 y1 di1 dj1)
      ; What corners of the rectangle correspond to hex centers is arranged
      ; in a checkerboard pattern.
      (if (eq? (even? i%) (even? j%))
        ; We have translated our space so the lower left corner is at the
        ; center of the 0,0 hexagon, so all rectangles congruent to 0,0
        ; checkerboard-wise will have hexagon centers in their lower left
        ; and upper right corners.
        (list 0 0 0 0
              rect-width rect-height 1 1)
        ; Thus, the others have hexagon centers in the lower right and upper
        ; left corners.
        (list rect-width 0 1 0
              0 rect-height 0 1)))
     ; Check which one is closest and obtain index offset.
     ((di dj) (if (< (squared-distance x%% y%% x% y%)
                     (squared-distance x%% y%% x1 y1))
                (list di0 dj0)
                (list di1 dj1))))
    (let ((j (+  dj j%)))
      ; Horizontally offset odd rows, divide by 2 because a rectangle is half
      ; an hexagon.
      (list (quotient (+ i% di (if (odd? j) -1 0))
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
         ; The position of the point relative to the bottom left corner of the
         ; i%,j% rectangle.  We'll use this to find the closest hexagon center
         ; and thus obtain a <column>,<row> offset from i%,j%.
         (x%% (- x% (* rect-width i%)))
         (y%% (- y% (* rect-height j%))))
    (list rect-width rect-height i% j% x%% y%%)))

; In counter-clockwise order, assuming center at 0,0 and radius 1.
(define hex-verts
  (let ((ir inner-radius))
    `((,ir -1/2) (,ir 1/2) (0 1) (,(- ir) 1/2) (,(- ir) -1/2) (0 -1))))

(define ((within-bounds? grid-size) cell)
  (bind-let (((grid-width grid-height) grid-size)
             ((i j) cell))
    (assert (positive-integers? grid-width grid-height))
    (and (<= 0 i) (< i grid-width)
         (<= 0 j) (< j grid-height))))

(def2 (distance grid-width grid-height grid-size)
  (lambda (cell other)
    (bind-let (((i0 j0) cell)
               ((i1 j1) other))
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
                       (- (ceiling di) (ceiling (/ dj 2))))))))))

(define distance-nowrap (distance '(#f #f)))

; The contrived name highlights that these are world coordinates.
(define (grid-world-size grid-size radius)
  (bind (grid-width grid-height) grid-size
    (list (* (+ (* grid-width 2)
                ; Offset for odd rows, if any.
                (if (> grid-height 1) 1 0))
             inner-radius radius)
          (* (+ 1/2 ; bottom half of first row.
                (* row-height grid-height))
             radius))))

; Calculate largest radius for which our grid will still fit in
; `container-size`, with `margin` units left at both sides in the most
; constrained direction.
(define (radius-to-fit grid-size container-size margin)
  (let* ((world-1 (grid-world-size grid-size 1))
         (canvas-size (map (lambda (x) (- x (* 2 margin))) container-size)))
    (apply min (map / canvas-size world-1))))

; Calculate world/screen coordinates for the 0,0 hexagon, such that the grid
; will be centered in `container-size`.
(define (origin-to-center grid-size hex-radius container-size)
  ; We choose the contrived name grid-world-(width|height) to highlight that
  ; these are screen/world coordinates.
  (bind-let (((grid-world-width grid-world-height)
              (grid-world-size grid-size hex-radius))
             ((container-width container-height) container-size))
    (list
      (+ (/ (- container-width grid-world-width) 2)
         (* inner-radius hex-radius))
      (+ (/ (- container-height grid-world-height) 2)
         hex-radius))))

; Utility.

(define (squared-distance x0 y0 x1 y1)
  (+ (square (- x1 x0)) (square (- y1 y0))))

(define (square x) (* x x))

(define (positive-integers? . l)
  (every (lambda (x) (and integer? x) (> x 0)) l))

