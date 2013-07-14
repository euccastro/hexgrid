(module hexgrid
  (directions
   distance
   distance-nowrap
   east
   grid->world
   grid-world-size
   hex-verts
   horizontal-wrapper
   indexer
   inner-radius
   make-grid-vector
   normalized-hex-verts
   northeast
   northwest
   origin-to-center
   radius-to-fit
   row-height
   southeast
   southwest
   vertical-wrapper
   west
   within-bounds?
   world->grid)
  (import scheme chicken srfi-1)
  (include "hex.scm"))

