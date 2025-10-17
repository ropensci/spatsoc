# Test fusion_id
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
  expect_error(fusion_id(),
  'edges must be')
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
    threshold = 'potato'
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

test_that('returns a numeric fusionID column', {
  edges[, fusionID := NULL]
  expect_contains(colnames(fusion_id(
    edges = edges,
    threshold = 50
  )), 'fusionID')
  expect_type(edges$fusionID, 'integer')
})

test_that('message if fusionID column already present, overwritten', {
  fusionID_present <- copy(edges)[, fusionID := 42]
  expect_message(fusion_id(
    edges = fusionID_present,
    threshold = 50
  ), 'fusionID column will be overwritten by this function')
})



test_that('allow_split TRUE returns less unique fusionID', {
  # When splits are allowed, more fusion events are combined
  #  resulting in less unique fusion events
  expect_lt(
    fusion_id(edges, allow_split = TRUE)[, uniqueN(fusionID)],
    fusion_id(edges, allow_split = FALSE)[, uniqueN(fusionID)]
  )
})

test_that('larger n_max_missing returns less unique fusionID', {
  # When a larger n max missing is provided, more fusion events are combined
  #  resulting in less unique fusion events
  expect_lt(
    fusion_id(edges, n_max_missing = 3)[, uniqueN(fusionID)],
    fusion_id(edges, n_max_missing = 0)[, uniqueN(fusionID)]
  )
})

test_that('larger n_min_length returns less unique fusionID', {
  # When a larger n min length is provided, less fusion events are identified
  #  resulting in less unique fusion events
  expect_lt(
    fusion_id(edges, n_min_length = 3)[, uniqueN(fusionID)],
    fusion_id(edges, n_min_length = 0)[, uniqueN(fusionID)]
  )
})

edges_expected <- data.table(
  dyadID = rep('A-B', 11),
  timegroup = seq.int(12)[-5],
  distance = c(1, 50, 1, 1, 1, 50, 50, 1, 1, 50, 1)
)
threshold <- 25

test_that('n_min_length returns expected number of unique fusionIDs', {
  expect_equal(
    fusion_id(
      edges_expected,
      threshold = threshold,
      n_min_length = 0
    )[, uniqueN(fusionID,  na.rm = TRUE)],
    5
  )

  expect_equal(
    fusion_id(
      edges_expected,
      threshold = threshold,
      n_min_length = 2
    )[, uniqueN(fusionID,  na.rm = TRUE)],
    2
  )

  expect_equal(
    fusion_id(
      edges_expected,
      threshold = threshold,
      n_min_length = 3
    )[, uniqueN(fusionID,  na.rm = TRUE)],
    0
  )

})


test_that('allow_split returns expected number of unique fusionIDs', {

  expect_equal(
    fusion_id(
      edges_expected,
      threshold = threshold,
      n_min_length = 2,
      allow_split = FALSE
    )[, uniqueN(fusionID,  na.rm = TRUE)],
    2
  )

  expect_equal(
    fusion_id(
      edges_expected,
      threshold = threshold,
      n_min_length = 2,
      allow_split = FALSE
    )[!is.na(fusionID), .N, fusionID][, max(N)],
    2
  )

  expect_equal(
    fusion_id(
      edges_expected,
      threshold = threshold,
      n_min_length = 2,
      allow_split = TRUE
    )[, uniqueN(fusionID,  na.rm = TRUE)],
    2
  )

  expect_equal(
    fusion_id(
      edges_expected,
      threshold = threshold,
      n_min_length = 2,
      allow_split = TRUE
    )[!is.na(fusionID), .N, fusionID][, max(N)],
    4
  )
})


test_that('n_max_missing returns expected number of unique fusionIDs', {
  expect_equal(
    fusion_id(
      edges_expected,
      threshold = threshold,
      n_min_length = 0,
      n_max_missing = 1,
      allow_split = FALSE
    )[, uniqueN(fusionID,  na.rm = TRUE)],
    4
  )

  expect_equal(
    fusion_id(
      edges_expected,
      threshold = threshold,
      n_min_length = 2,
      n_max_missing = 1,
      allow_split = FALSE
    )[, uniqueN(fusionID,  na.rm = TRUE)],
    2
  )

  expect_equal(
    fusion_id(
      edges_expected,
      threshold = threshold,
      n_min_length = 2,
      n_max_missing = 1,
      allow_split = FALSE
    )[!is.na(fusionID), .N, fusionID][, max(N)],
    3
  )

  expect_equal(
    fusion_id(
      edges_expected,
      threshold = threshold,
      n_min_length = 2,
      n_max_missing = 1,
      allow_split = TRUE
    )[, uniqueN(fusionID,  na.rm = TRUE)],
    2
  )

  expect_equal(
    fusion_id(
      edges_expected,
      threshold = threshold,
      n_min_length = 2,
      n_max_missing = 1,
      allow_split = TRUE
    )[!is.na(fusionID), .N, fusionID][, max(N)],
    5
  )

})
