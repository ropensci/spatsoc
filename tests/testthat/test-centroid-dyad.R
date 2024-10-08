# Test centroid_dyad
context('test centroid_dyad')

library(spatsoc)

DT <- fread('../testdata/DT.csv')
id <- 'ID'
datetime <- 'datetime'
timethreshold <- '20 minutes'
threshold <- 50
coords <- c('X', 'Y')
timegroup <- 'timegroup'
group <- 'group'


DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, threshold = timethreshold)
edges <- edge_dist(DT, threshold = threshold, id = id, coords = coords,
                   timegroup = timegroup, returnDist = TRUE, fillNA = FALSE)
dyad_id(edges, id1 = 'ID1', id2 = 'ID2')

clean_DT <- copy(DT)
clean_edges <- copy(edges)

test_that('edges, DT are required', {
  expect_error(centroid_dyad(edges, DT = NULL))
  expect_error(centroid_dyad(edges = NULL, DT))
})

test_that('arguments required, otherwise error detected', {
  expect_error(centroid_dyad(edges, DT, id = id, coords = 'X'),
               'coords requires a vector')
  expect_error(centroid_dyad(edges, DT, id = NULL),
               'id column name required')
  expect_error(centroid_dyad(edges, DT, id = id, coords = coords, timegroup = NULL),
               'timegroup column name required')
  expect_error(centroid_dyad(edges, DT, id = id, coords = coords, na.rm = NULL),
               'na.rm is required')
})

test_that('na.rm is boolean', {
  expect_error(centroid_dyad(edges, DT, id = id, coords = coords, na.rm = 'potato'),
               'boolean')
})

test_that('column names must exist in DT', {
  expect_error(centroid_dyad(edges, DT, id = 'potato', coords = coords),
               'potato field')
  expect_error(centroid_dyad(edges, DT, id = id, coords = rep('potato', 2)),
               'potato field')
  expect_error(centroid_dyad(edges, DT, id = id, coords = coords, timegroup = 'potato'),
               'potato field')
})

test_that('coords are correctly provided or error detected', {
  expect_error(centroid_dyad(edges, DT, id = id, coords = c('X', NULL)),
               'coords requires a vector')
  expect_error(centroid_dyad(edges, DT, id = id, coords = c('X', 'ID')),
               'coords must be numeric')
})

test_that('centroid column succesfully detected', {
  copyEdges <- copy(edges)[, centroid_X := 1]
  expect_message(
    centroid_dyad(copyEdges, DT, id = id, coords = coords),
    'centroid_X column will be overwritten'
  )
})

test_that('no rows are added to the result DT', {
  expect_equal(nrow(edges),
               nrow(centroid_dyad(edges, DT, id = id, coords = coords)))
})

test_that('two columns added to the result DT', {
  copyEdges <- copy(edges)

  expect_equal(ncol(copyEdges) + 2,
               ncol(centroid_dyad(edges, DT, id = id, coords = coords)))
})

test_that('two columns added to the result DT are doubles', {
  expect_type(centroid_dyad(edges, DT, id = id, coords = coords)$centroid_X, 'double')
  expect_type(centroid_dyad(edges, DT, id = id, coords = coords)$centroid_Y, 'double')
})

test_that('returns a data.table', {
  expect_s3_class(centroid_dyad(edges, DT, id = id, coords = coords), 'data.table')
})

expected_DT <- copy(clean_DT)[timegroup < 3]
expected_DT[, X := timegroup * 10 + .I]
expected_DT[, Y := timegroup * 10]

expected_edges <- copy(clean_edges)[timegroup < 3]
expected_DT[ID %in% first(expected_edges$ID1), X := NA]

test_that('results are expected', {
  expect_equal(
    centroid_dyad(expected_edges, expected_DT, id = id, coords = coords,
                  na.rm = FALSE)[timegroup == 1, unique(centroid_X)],
    NA_real_
  )

  expect_equal(
    centroid_dyad(expected_edges, expected_DT, id = id, coords = coords,
                  na.rm = TRUE)[timegroup == 1, unique(centroid_Y)],
    10
  )

  expect_equal(
    centroid_dyad(expected_edges, expected_DT, id = id, coords = coords,
                  na.rm = FALSE)[timegroup == 2, .N],
    0
  )

  expect_gt(
    centroid_dyad(expected_edges, expected_DT, id = id, coords = coords,
                  na.rm = TRUE)[timegroup == 1, unique(centroid_X)],
    10
  )

  expect_equal(
    expected_edges[, .N],
    centroid_dyad(expected_edges, expected_DT, id = id, coords = coords)[, .N]
  )

  expect_equal(
    expected_edges[, unique(ID1)],
    centroid_dyad(expected_edges, expected_DT, id = id, coords = coords)[, unique(ID1)]
  )
})

