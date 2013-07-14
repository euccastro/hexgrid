(use test bindings srfi-1 hexgrid)

(define (run-tests)

  (define grid-width 3)
  (define grid-height 4)
  (define grid-size (list grid-width grid-height))

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
  (define origin '(0 0))
  (test "east even" '(1 0) (east origin))
  (test "northeast even" '(0 1) (northeast origin))
  (test "northwest even" '(-1 1) (northwest origin))
  (test "west even" '(-1 0) (west origin))
  (test "southwest even" '(-1 -1) (southwest origin))
  (test "southeast even" '(0 -1) (southeast origin))
  (test "east odd" '(1 1) (east '(0 1)))
  (test "northeast odd" '(1 2) (northeast '(0 1)))
  (test "northwest odd" '(0 2) (northwest '(0 1)))
  (test "west odd" '(-1 1) (west '(0 1)))
  (test "southwest odd" '(0 0) (southwest '(0 1)))
  (test "southeast odd" '(1 0) (southeast '(0 1)))

  ; Set and read.
  (let* ((vec (make-grid-vector grid-size))
         (index (indexer grid-size))
         (hwrap (horizontal-wrapper grid-width))
         (vwrap (vertical-wrapper grid-height))
         (wrapindex (compose index hwrap vwrap))
         (cell '(-1 -1)))
    (vector-set! vec (wrapindex cell) 'hello)
    (test 'hello (vector-ref vec (wrapindex cell)))
    (test 'hello (vector-ref vec (index '(2 3)))))

  ; grid->world
  (define g0->w (grid->world origin 1))
  (test "grid->world minimal" '(0.0 0.0) (g0->w origin))
  (test "grid->world move origin" '(3.0 4.0) ((grid->world '(3 4) 1) origin))
  (test "grid->world move x"
        (list (* inner-radius 2) 0.0)
        (g0->w '(1 0)))
  (test "grid->world move y (odd)"
        (list inner-radius row-height)
        (g0->w '(0 1)))
  (test "grid->world negative x"
        (list (* inner-radius -4) 0.0)
        (g0->w '(-2 0)))
  (test "grid->world negative y"
        (list (* inner-radius -2) (* row-height -2))
        (g0->w '(-1 -2)))

  ; world->grid
  (define w0->g (world->grid origin 1))
  (test "world->grid basic" origin (w0->g origin))
  (test "world->grid move x" '(1 0) (w0->g '(1 0)))
  (test "world->grid move origin x"
        '(-1 0)
        ((world->grid '(1 0) 1) origin))
  (test "world->grid move origin y"
        '(0 -1)
        ((world->grid '(0.0 1.2) 1) '(0.2 0)))
  (let ((sectors
  '(("a" 0.5 0.2) ("b" 0.0 0.5) ("c" -0.5 0.2)
                  ("d" -0.5 -0.2) ("e" 0.0 -0.5) ("f" 0.5 -0.2)))
        (cells `(("even" 0 0 0 0) ("odd" 0 1 ,inner-radius ,row-height))))
    (for-each
      (lambda (sector)
        (for-each
          (lambda (cell)
            (bind (name expi expj offsetx offsety) cell
              (bind (sector-name sectorx sectory) sector
                (test (string-append name "-" sector-name)
                      (list expi expj)
                      (w0->g
                        (list (+ offsetx sectorx)
                              (+ offsety sectory)))))))
          cells))
      sectors))

  ; Distance (no wrap-around shortcuts).
  (test 0 (distance-nowrap '(0 0) '(0 0)))
  (test 1 (distance-nowrap '(0 0) '(1 0)))
  (test 2 (distance-nowrap '(0 0) '(2 0)))
  (test 1 (distance-nowrap '(0 0) '(0 1)))
  (test 2 (distance-nowrap '(0 0) '(1 1)))
  (test 3 (distance-nowrap '(0 0) '(2 1)))
  (test 2 (distance-nowrap '(0 0) '(0 2)))
  (test 2 (distance-nowrap '(0 0) '(1 2)))
  (test 3 (distance-nowrap '(0 0) '(2 2)))
  (test 3 (distance-nowrap '(0 0) '(0 3)))
  (test 3 (distance-nowrap '(0 0) '(1 3)))
  (test 4 (distance-nowrap '(0 0) '(2 3)))
  ; You can get distance to out-of-bounds coordinates without providing width
  ; and height.  They will be calculated as though those coordinates were
  ; within the grid, but without cutting around.  So distance to -1,-1 is
  ; smaller than to 2,3 even though they would be the same hex in
  ; a wrap-around grid.
  (test 1 (distance-nowrap '(0 0)  '(-1 -1)))

  ; Distance considering wrap-around shourtcuts.
  (for-each
    (lambda (l)
      (bind (i j d) l
        (test d ((distance grid-size) '(0 0) (list i j)))))
    '((0 0 0)
      (1 0 1) (-1 0 1) (0 1 1) (-1 1 1) (0 -1 1) (-1 -1 1) (2 3 1)
      (1 1 2) (0 2 2) (1 2 2) (2 2 2) (1 3 2)))

  (test "grid-world-size trivial" (list (* 2 inner-radius) 2.0)
        (grid-world-size '(1 1) 1))
  (test "grid-world-size" (list (* 21 inner-radius) 19.5)
        (grid-world-size (list grid-width grid-height) 3))
  (test-exit))

(run-tests)
