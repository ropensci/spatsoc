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


test_that('columns are required otherwise error detected', {
  expect_error(fusion_id(
    edges = edges[, .SD, .SDcols = -'dyadID']
  ))
  expect_error(fusion_id(
    edges = edges[, .SD, .SDcols = -'timegroup']
  ))
  expect_error(fusion_id(
    edges = edges[, .SD, .SDcols = -'distance']
  ))
})

test_that('arguments are correctly provided or error detected', {
  expect_error(fusion_id(
    edges = edges,
    threshold = NULL
  ))
  expect_error(fusion_id(
    edges = edges,
    threshold = -42
  ))
  expect_error(fusion_id(
    edges = edges,
    threshold = "potato"
  ))

  expect_error(fusion_id(
    edges = edges,
    n_min_length = 'potato'
  ))

  expect_error(fusion_id(
    edges = edges,
    n_max_missing = 'potato'
  ))
  expect_error(fusion_id(
    edges = edges,
    allow_split = 'potato'
  ))
  expect_error(fusion_id(
    edges = edges,
    allow_split = 42
  ))
})

test_that('returns a data.table', {
  expect_s3_class(fusion_id(
    edges = edges,
    threshold = 50
  ), 'data.table')
})
