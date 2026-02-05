# Test fusion_id
context("test fusion_id")

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
    fusion_id(edges, allow_split = TRUE, threshold = 50)[, uniqueN(fusionID)],
    fusion_id(edges, allow_split = FALSE, threshold = 50)[, uniqueN(fusionID)]
  )
})

test_that('larger n_max_missing returns less unique fusionID', {
  # When a larger n max missing is provided, more fusion events are combined
  #  resulting in less unique fusion events
  expect_lt(
    fusion_id(edges, n_max_missing = 3, threshold = 50)[, uniqueN(fusionID)],
    fusion_id(edges, n_max_missing = 0, threshold = 50)[, uniqueN(fusionID)]
  )
})

test_that('larger n_min_length returns less unique fusionID', {
  # When a larger n min length is provided, less fusion events are identified
  #  resulting in less unique fusion events
  expect_lt(
    fusion_id(edges, n_min_length = 3, threshold = 50)[, uniqueN(fusionID)],
    fusion_id(edges, n_min_length = 0, threshold = 50)[, uniqueN(fusionID)]
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
      n_min_length = 0,
      n_max_missing = 0,
      allow_split = FALSE
    )[, uniqueN(fusionID,  na.rm = TRUE)],
    5
  )

  expect_equal(
    fusion_id(
      edges_expected,
      threshold = threshold,
      n_min_length = 2,
      n_max_missing = 0,
      allow_split = FALSE
    )[, uniqueN(fusionID,  na.rm = TRUE)],
    2
  )

  expect_equal(
    fusion_id(
      edges_expected,
      threshold = threshold,
      n_min_length = 3,
      n_max_missing = 0,
      allow_split = FALSE
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
      n_max_missing = 0,
      allow_split = FALSE
    )[, uniqueN(fusionID,  na.rm = TRUE)],
    2
  )

  expect_equal(
    fusion_id(
      edges_expected,
      threshold = threshold,
      n_min_length = 2,
      n_max_missing = 0,
      allow_split = FALSE
    )[!is.na(fusionID), .N, fusionID][, max(N)],
    2
  )

  expect_equal(
    fusion_id(
      edges_expected,
      threshold = threshold,
      n_min_length = 2,
      n_max_missing = 0,
      allow_split = TRUE
    )[, uniqueN(fusionID,  na.rm = TRUE)],
    2
  )

  expect_equal(
    fusion_id(
      edges_expected,
      threshold = threshold,
      n_min_length = 2,
      n_max_missing = 0,
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


test_that('lead edge cases identified as expected', {
  split_expected <- data.table(
    dyadID = rep('A-B', 7),
    timegroup = c(1,  2,   3, 10, 11, 12,  13),
    distance =  c(50, 1,  50, 1,  1,  50,   1)
  )
  threshold <- 25

  # If allow_split = TRUE,
  #  obs separate (tg 3) where not within 1 tg of lead+lag together should be NA
  #  obs separate (tg 12) where within lead+lag obs together should be same fusionID
  split_allow_true <- fusion_id(
    copy(split_expected),
    threshold = threshold,
    n_min_length = 0,
    n_max_missing = 0,
    allow_split = TRUE
  )
  expect_identical(split_allow_true[, fusionID[timegroup == 3]], NA_integer_)
  expect_false(identical(split_allow_true[, fusionID[timegroup == 3]],
                         split_allow_true[, fusionID[timegroup == 2]]))
  expect_false(identical(split_allow_true[, fusionID[timegroup == 3]],
                         split_allow_true[, fusionID[timegroup == 11]]))
  expect_identical(split_allow_true[, fusionID[timegroup == 11]],
                   split_allow_true[, fusionID[timegroup == 12]])

  # If allow_split = TRUE and n_max_missing = 7,
  #  obs separate (tg 3) where within lead+lag obs together should be
  #   same fusionID given n_max_missing = 7
  #  obs separate (tg 12) where within lead+lag obs together should be same fusionID
  split_allow_true_7_miss <- fusion_id(
    copy(split_expected),
    threshold = threshold,
    n_min_length = 0,
    n_max_missing = 7,
    allow_split = TRUE
  )
  expect_identical(split_allow_true_7_miss[, fusionID[timegroup == 3]],
                   split_allow_true_7_miss[, fusionID[timegroup == 2]])
  expect_identical(split_allow_true_7_miss[, fusionID[timegroup == 3]],
                   split_allow_true_7_miss[, fusionID[timegroup == 11]])
  expect_identical(split_allow_true_7_miss[, fusionID[timegroup == 11]],
                   split_allow_true_7_miss[, fusionID[timegroup == 12]])

  # Also check with n_min_length
  split_allow_true <- fusion_id(
    copy(split_expected),
    threshold = threshold,
    n_min_length = 1,
    n_max_missing = 0,
    allow_split = TRUE
  )
  expect_identical(split_allow_true[, fusionID[timegroup == 3]], NA_integer_)
  expect_false(identical(split_allow_true[, fusionID[timegroup == 3]],
                         split_allow_true[, fusionID[timegroup == 2]]))
  expect_false(identical(split_allow_true[, fusionID[timegroup == 3]],
                         split_allow_true[, fusionID[timegroup == 11]]))
  expect_identical(split_allow_true[, fusionID[timegroup == 11]],
                   split_allow_true[, fusionID[timegroup == 12]])


  lead_expected <- data.table(
    dyadID = rep('A-B', 5),
    timegroup = c(1,   3, 4, 5, 6),
    distance =  c(50,  1, 1, 1, 1)
  )
  threshold <- 25

  # If defaults,
  #  first obs together (tg 3) where lag timegroup difference is > 1, but
  #  lead timegroup difference is 1, fusionID should match tg 4-6s
  defaults_lead <- fusion_id(
    copy(lead_expected),
    threshold = threshold,
    n_min_length = 0,
    n_max_missing = 0,
    allow_split = FALSE
  )

  expect_identical(defaults_lead[, fusionID[timegroup == 3]],
                   defaults_lead[, fusionID[timegroup == 4]])
  expect_identical(defaults_lead[, fusionID[timegroup == 3]],
                   defaults_lead[, fusionID[timegroup == 5]])
  expect_identical(defaults_lead[, fusionID[timegroup == 3]],
                   defaults_lead[, fusionID[timegroup == 6]])

  # Also check with n_max_missing
  defaults_lead <- fusion_id(
    copy(lead_expected),
    threshold = threshold,
    n_min_length = 0,
    n_max_missing = 2,
    allow_split = FALSE
  )

  expect_identical(defaults_lead[, fusionID[timegroup == 3]],
                   defaults_lead[, fusionID[timegroup == 4]])
  expect_identical(defaults_lead[, fusionID[timegroup == 3]],
                   defaults_lead[, fusionID[timegroup == 5]])
  expect_identical(defaults_lead[, fusionID[timegroup == 3]],
                   defaults_lead[, fusionID[timegroup == 6]])

  # Also check with n_max_missing, n_min_length
  defaults_lead <- fusion_id(
    copy(lead_expected),
    threshold = threshold,
    n_min_length = 2,
    n_max_missing = 2,
    allow_split = FALSE
  )

  expect_identical(defaults_lead[, fusionID[timegroup == 3]],
                   defaults_lead[, fusionID[timegroup == 4]])
  expect_identical(defaults_lead[, fusionID[timegroup == 3]],
                   defaults_lead[, fusionID[timegroup == 5]])
  expect_identical(defaults_lead[, fusionID[timegroup == 3]],
                   defaults_lead[, fusionID[timegroup == 6]])

})
