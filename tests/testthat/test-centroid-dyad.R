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

# Note: since centroid_dyad and centroid_fusion share an internal function
#       that orchestrates the centroid calculation for both, testing is
#       overlapping. See test-centroid-fusion for full testing suite.

expected_DT <- copy(clean_DT)[timegroup < 3]
expected_DT[, X := timegroup * 10 + .I]
expected_DT[, Y := timegroup * 10]

expected_edges <- copy(clean_edges)[timegroup < 3]
expected_DT[ID %in% first(expected_edges$ID1), X := NA]

test_that('results are expected', {
  expect_equal(
    centroid_dyad(expected_edges, expected_DT, id = id, coords = coords)[
      timegroup == 1, unique(centroid_Y)],
    10
  )

  expect_equal(
    centroid_dyad(expected_edges, expected_DT, id = id, coords = coords)[
      timegroup == 2, .N],
    0
  )

  expect_gt(
    centroid_dyad(expected_edges, expected_DT, id = id, coords = coords)[
      timegroup == 1, unique(centroid_X)],
    10
  )

  expect_equal(
    expected_edges[, .N],
    centroid_dyad(expected_edges, expected_DT, id = id, coords = coords)[, .N]
  )

  expect_equal(
    expected_edges[, unique(ID1)],
    centroid_dyad(expected_edges, expected_DT,
                  id = id, coords = coords)[, unique(ID1)]
  )
})

test_that('NAs in dyadID result in NAs for centroid', {
  # Set fillNA = TRUE
  edges <- edge_dist(DT, threshold = threshold, id = id, coords = coords,
                     timegroup = timegroup, returnDist = TRUE,
                     fillNA = TRUE)
  dyad_id(edges, id1 = 'ID1', id2 = 'ID2')

  centroids <- centroid_dyad(edges, DT, id, coords)
  expect_true(
    centroids[is.na(dyadID), all(is.na(centroid_X))]
  )
  expect_true(
    centroids[is.na(dyadID), all(is.na(centroid_Y))]
  )
})
