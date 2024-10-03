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
