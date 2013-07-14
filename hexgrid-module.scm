(module hexgrid
  (directions
   distance
   distance-nowrap
   east
   grid->world
   hex-verts
   horizontal-wrapper
   indexer
   inner-radius
   make-grid-vector
   northeast
   northwest
   row-height
   southeast
   southwest
   vertical-wrapper
   west
   within-bounds?
   world->grid
   world-size)
  (import scheme chicken srfi-1)
  (include "hex.scm"))

