# Test centroid_fusion
context('test centroid_fusion')

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
fusion_id(edges, threshold = threshold)

clean_DT <- copy(DT)
clean_edges <- copy(edges)

test_that('edges, DT are required', {
  expect_error(centroid_fusion(edges, DT = NULL))
  expect_error(centroid_fusion(edges = NULL, DT))
})

test_that('arguments required, otherwise error detected', {
  expect_error(centroid_fusion(edges, DT, id = id, coords = 'X'),
               'coords must be length 2')
  expect_error(centroid_fusion(edges, DT, id = NULL),
               'id must be provided')
  expect_error(centroid_fusion(edges, DT, id = id,
                               coords = coords, timegroup = NULL),
               'timegroup must be provided')
})

test_that('column names must exist in DT', {
  expect_error(centroid_fusion(edges, DT, id = 'potato', coords = coords),
               'potato field')
  expect_error(centroid_fusion(edges, DT, id = id, coords = rep('potato', 2)),
               'potato field')
  expect_error(centroid_fusion(edges, DT, id = id,
                               coords = coords, timegroup = 'potato'),
               'potato field')
})

test_that('coords are correctly provided or error detected', {
  expect_error(centroid_fusion(edges, DT, id = id, coords = c('X', NULL)),
               'coords must be length 2')
  expect_error(centroid_fusion(edges, DT, id = id, coords = c('X', 'ID')),
               'coords must be of class numeric')
})

test_that('centroid column succesfully detected', {
  copyEdges <- copy(edges)[, centroid_X := 1]
  expect_message(
    centroid_fusion(copyEdges, DT, id = id, coords = coords),
    'centroid_X column will be overwritten'
  )
  copyEdges <- copy(edges)[, centroid_Y := 1]
  expect_message(
    centroid_fusion(copyEdges, DT, id = id, coords = coords),
    'centroid_Y column will be overwritten'
  )
})

test_that('no rows are added to the result DT', {
  expect_equal(nrow(edges),
               nrow(centroid_fusion(edges, DT, id = id, coords = coords)))
})

test_that('two columns added to the result DT', {
  copyEdges <- copy(edges)

  expect_equal(ncol(copyEdges) + 2,
               ncol(centroid_fusion(edges, DT, id = id, coords = coords)))
})

test_that('two columns added to the result DT are doubles', {
  expect_type(centroid_fusion(edges, DT,
                              id = id, coords = coords)$centroid_X, 'double')
  expect_type(centroid_fusion(edges, DT,
                              id = id, coords = coords)$centroid_Y, 'double')
})

test_that('returns a data.table', {
  expect_s3_class(centroid_fusion(edges, DT,
                                  id = id, coords = coords), 'data.table')
})

expected_DT <- copy(clean_DT)[timegroup < 10]
expected_DT[, X := timegroup * 10 + .I]
expected_DT[, Y := timegroup * 10]

expected_edges <- copy(clean_edges)[timegroup < 10]
expected_DT[ID %in% first(expected_edges$ID1), X := NA]

test_that('results are expected', {
  expect_equal(
    centroid_fusion(expected_edges, expected_DT, id = id, coords = coords)[
      timegroup == 1, unique(centroid_Y)],
    10
  )

  expect_equal(
    centroid_fusion(expected_edges, expected_DT, id = id, coords = coords)[
      timegroup == 2, .N],
    0
  )

  expect_gt(
    centroid_fusion(expected_edges, expected_DT, id = id, coords = coords)[
                    timegroup == 1, unique(centroid_X)],
    10
  )

  expect_equal(
    expected_edges[, .N],
    centroid_fusion(expected_edges, expected_DT,
                    id = id, coords = coords)[, .N]
  )

  expect_equal(
    expected_edges[, unique(ID1)],
    centroid_fusion(expected_edges, expected_DT,
                    id = id, coords = coords)[, unique(ID1)]
  )

  expect_gte(
    centroid_fusion(expected_edges, expected_DT, id = id, coords = coords)[
      , uniqueN(centroid_X)],
    expected_edges[, uniqueN(fusionID)]
  )

  expect_gte(
    centroid_fusion(expected_edges, expected_DT, id = id, coords = coords)[
      , uniqueN(centroid_Y)],
    expected_edges[, uniqueN(fusionID)]
  )
})



test_that('NAs in fusionID result in NAs for centroid', {
  # Set fillNA = TRUE
  edges <- edge_dist(DT, threshold = threshold, id = id, coords = coords,
                     timegroup = timegroup, returnDist = TRUE,
                     fillNA = TRUE)
  dyad_id(edges, id1 = 'ID1', id2 = 'ID2')
  fusion_id(edges, threshold = threshold)

  centroids <- centroid_fusion(edges, DT, id, coords)
  expect_true(
    centroids[is.na(fusionID), all(is.na(centroid_X))]
  )
  expect_true(
    centroids[is.na(fusionID), all(is.na(centroid_Y))]
  )

})
