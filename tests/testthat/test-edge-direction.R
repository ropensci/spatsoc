# Test edge_direction
context('test edge_direction')

library(spatsoc)

DT <- fread('../testdata/DT.csv')
id <- 'ID'
datetime <- 'datetime'
timethreshold <- '20 minutes'
threshold <- 50
coords <- c('X', 'Y')
timegroup <- 'timegroup'
group <- 'group'
projection <- 32736


DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = datetime, threshold = timethreshold)
edges <- edge_dist(DT, threshold = threshold, id = id, coords = coords,
                   timegroup = timegroup, returnDist = TRUE, fillNA = FALSE)
dyad_id(edges, id1 = 'ID1', id2 = 'ID2')

clean_DT <- copy(DT)
clean_edges <- copy(edges)

test_that('edges, DT are required', {
  expect_error(edge_direction(edges, DT = NULL))
  expect_error(edge_direction(edges = NULL, DT))
})

test_that('arguments required, otherwise error detected', {
  expect_error(edge_direction(edges, DT, id = id, coords = 'X'),
               'coords requires a vector')
  expect_error(edge_direction(edges, DT, id = NULL), 'id column name required')
  expect_error(
    edge_direction(
      edges,
      DT,
      id = id,
      coords = coords,
      timegroup = NULL
    ),
    'timegroup required'
  )
  expect_error(
    edge_direction(
      edges,
      DT,
      id = id,
      coords = coords,
      timegroup = timegroup,
      projection = NULL
    ),
    'projection required'
  )
})

test_that('column names must exist in DT', {
  expect_error(edge_direction(edges, DT, id = 'potato', coords = coords),
               'potato field')
  expect_error(edge_direction(edges, DT, id = id, coords = rep('potato', 2)),
               'potato field')
  expect_error(edge_direction(
    edges,
    DT,
    id = id,
    coords = coords,
    timegroup = 'potato'
  ),
  'potato field')
})

test_that('coords are correctly provided or error detected', {
  expect_error(edge_direction(edges, DT, id = id, coords = c('X', NULL)),
               'coords requires a vector')
  expect_error(edge_direction(edges, DT, id = id, coords = c('X', 'ID')),
               'coords must be numeric')
})

test_that('direction_dyad column succesfully detected', {
  copyEdges <- copy(edges)[, direction_dyad := 1]
  expect_message(
    edge_direction(
      copyEdges,
      DT,
      id = id,
      coords = coords,
      projection = projection,
      timegroup = timegroup
    ),
    'direction_dyad column will be overwritten'
  )
})
