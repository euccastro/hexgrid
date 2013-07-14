(module hexgrid
  (directions
   distance
   east
   grid->world
   hex-verts
   index
   inner-radius
   make-grid-vector
   northeast
   northwest
   row-height
   southeast
   southwest
   west
   within-bounds?
   world->grid
   world-size
   wrap)
  (import scheme chicken srfi-1)
  (include "hex.scm"))

