(use test bindings)
(load "hexgrid-module.scm")
(import hexgrid)

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
    (test 'hello (vector-ref vec (index grid-width grid-height -1 -1 #t)))
    (test 'hello (vector-ref vec (index grid-width grid-height 2 3))))

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
  (test 0 (distance 0 0 0 0))
  (test 1 (distance 0 0 1 0))
  (test 2 (distance 0 0 2 0))
  (test 1 (distance 0 0 0 1))
  (test 2 (distance 0 0 1 1))
  (test 3 (distance 0 0 2 1))
  (test 2 (distance 0 0 0 2))
  (test 2 (distance 0 0 1 2))
  (test 3 (distance 0 0 2 2))
  (test 3 (distance 0 0 0 3))
  (test 3 (distance 0 0 1 3))
  (test 4 (distance 0 0 2 3))
  ; You can get distance to out-of-bounds coordinates without providing width
  ; and height.  They will be calculated as though those coordinates were
  ; within the grid, but without cutting around.  So distance to -1,-1 is
  ; smaller than to 2,3 even though they would be the same hex in
  ; a wrap-around grid.
  (test 1 (distance 0 0 -1 -1))

  ; Distance considering wrap-around shourtcuts.
  (for-each
    (lambda (l)
      (bind (i j d) l
            (test d
                  (distance 0 0 i j
                            grid-width: grid-width grid-height: grid-height))))
    '((0 0 0)
      (1 0 1) (-1 0 1) (0 1 1) (-1 1 1) (0 -1 1) (-1 -1 1) (2 3 1)
      (1 1 2) (0 2 2) (1 2 2) (2 2 2) (1 3 2)))

  (test-values "world-size trivial" (* 2 inner-radius) 2.0
               world-size 1 1 1)
  (test-values "world-size" (* 21 inner-radius) 19.5
               world-size grid-width grid-height 3)
  (test-exit))

(define (test-values s expa expb f . rest)
  (receive (x y) (apply f rest)
    (test (string-append s " (x)") expa x)
    (test (string-append s " (y)") expb y)))

(run-tests)
