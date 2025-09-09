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

test_that('no rows are added to the result DT', {
  expect_equal(nrow(edges), nrow(
    edge_direction(
      edges,
      DT,
      id = id,
      coords = coords,
      projection = projection,
      timegroup = timegroup
    )
  ))
})

test_that('one columns added to the result DT', {
  copyEdges <- copy(edges)

  expect_equal(ncol(copyEdges) + 1, ncol(
    edge_direction(
      edges,
      DT,
      id = id,
      coords = coords,
      projection = projection,
      timegroup = timegroup
    )
  ))
})

test_that('column added to the result DT is unit', {
  expect_s3_class(
    edge_direction(
      edges,
      DT,
      id = id,
      coords = coords,
      projection = projection,
      timegroup = timegroup
    )$direction_dyad,
    'units'
  )
})

test_that('returns a data.table', {
  expect_s3_class(
    edge_direction(
      edges,
      DT,
      id = id,
      coords = coords,
      projection = projection,
      timegroup = timegroup
    ),
    'data.table'
  )
})


n <- 4
DT_B <- data.table(
  X = c(0, 5, 5, 0),
  Y = c(0, 0, 5, 5),
  timegroup = rep(1, n),
  ID = LETTERS[seq.int(n)]
)
edges_B <- edge_dist(DT_B, id = id, coords = coords, timegroup = timegroup,
                     threshold = NULL, returnDist = TRUE)
dyad_id(edges_B, 'ID1', 'ID2')

dyad_dirs <- edge_direction(edges_B, DT_B, id = id,
                            coords = coords, projection = 4326)

test_that('East North West South dyads', {
  tolerance <- 0.01

  expect_equal(
    dyad_dirs[dyadID == 'A-B' & ID1 == 'A', direction_dyad],
    as_units(pi / 2, 'rad'),
    tolerance = tolerance
  )

  expect_equal(
    dyad_dirs[dyadID == 'A-C' & ID1 == 'A', direction_dyad],
    as_units(pi / 4, 'rad'),
    tolerance = tolerance
  )

  expect_equal(
    dyad_dirs[dyadID == 'A-D' & ID1 == 'A', direction_dyad],
    as_units(0, 'rad'),
    tolerance = tolerance
  )

  expect_equal(
    dyad_dirs[dyadID == 'A-B' & ID1 == 'B', direction_dyad],
    -1 * as_units(pi / 2, 'rad'),
    tolerance = tolerance
  )

  expect_equal(
    dyad_dirs[dyadID == 'B-C' & ID1 == 'C', direction_dyad],
    as_units(pi, 'rad'),
    tolerance = tolerance
  )

})


