# Test edge_alignment
context('test-edge-alignment')

library(spatsoc)

id <- 'ID'
coords <- c('X', 'Y')
direction <- 'direction'
timegroup <- 'timegroup'
group <- 'group'
projection <- 32736

DT <- fread('../testdata/DT.csv')
group_times(DT, 'datetime', '10 minutes')
direction_step(
  DT = DT,
  id = id,
  coords = coords,
  projection = projection
)
