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

test_that('required arguments are provided else error', {
  expect_error(edge_alignment(DT = NULL),
               'DT')

  expect_error(edge_alignment(DT, id = NULL),
               'id')

  expect_error(edge_alignment(DT, id, direction = NULL),
               'direction')

  expect_error(edge_alignment(DT, id, direction, timegroup = NULL),
               'timegroup')
})
