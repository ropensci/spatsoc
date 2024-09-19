context("test-fusion-id")

library(spatsoc)

DT <- fread('../testdata/DT.csv')

group_times(DT, datetime = 'datetime', threshold = '20 minutes')

edges <- edge_dist(
    DT,
    threshold = 100,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup',
    returnDist = TRUE,
    fillNA = TRUE
  )

dyad_id(edges, 'ID1', 'ID2')

test_that('edges is required', {
  expect_error(fusion_id(
    edges = NULL
  ),
  'input edges required')
})
