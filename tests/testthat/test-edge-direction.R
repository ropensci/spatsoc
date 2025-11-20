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
utm <- 32736


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
               'coords must be length 2')
  expect_error(edge_direction(edges, DT, id = NULL), 'id must be provided')

  expect_error(
    edge_direction(
      edges,
      DT,
      id = id,
      coords = coords,
      timegroup = NULL
    ),
    'timegroup must be'
  )
  expect_error(
    edge_direction(
      edges,
      DT,
      id = id,
      coords = coords,
      timegroup = timegroup,
      crs = NULL
    ),
    'crs must be'
  )
})

test_that('column names must exist in DT', {
  expect_error(edge_direction(edges, DT, id = 'potato', coords = coords,
                              crs = utm),
               'potato field')
  expect_error(edge_direction(edges, DT, id = id, coords = rep('potato', 2),
                              crs = utm),
               'potato field')
  copyDT <- copy(DT)[, timegroup := NULL]
  expect_error(edge_direction(
    edges,
    copyDT,
    id = id,
    coords = coords,
    crs = utm,
    timegroup = 'timegroup'
  ),
  'timegroup field')

  copyEdges <- copy(edges)[, timegroup := NULL]
  expect_error(
    edge_direction(
      copyEdges,
      DT,
      id = id,
      coords = coords,
      crs = utm,
      timegroup = timegroup
    ),
    'timegroup field'
  )

  copyEdges <- copy(edges)[, dyadID := NULL]
  expect_error(
    edge_direction(
      copyEdges,
      DT,
      id = id,
      coords = coords,
      crs = utm,
      timegroup = timegroup
    ),
    'dyadID field'
  )

})

test_that('coords are correctly provided or error detected', {
  expect_error(edge_direction(edges, DT, id = id, coords = c('X', NULL)),
               'coords must be length 2')
  expect_error(edge_direction(edges, DT, id = id, coords = c('X', 'ID')),
               'coords must be of class numeric')
})

test_that('direction_dyad column succesfully detected', {
  copyEdges <- copy(edges)[, direction_dyad := 1]
  expect_message(
    edge_direction(
      copyEdges,
      DT,
      id = id,
      coords = coords,
      crs = utm,
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
      crs = utm,
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
      crs = utm,
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
      crs = utm,
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
      crs = utm,
      timegroup = timegroup
    ),
    'data.table'
  )
})

test_that('projection arg is deprecated', {
  expect_warning(
    edge_direction(
      edges,
      DT,
      id = id,
      coords = coords,
      timegroup = timegroup,
      projection = utm
    ),
    'projection argument is deprecated'
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
                            coords = coords, crs = 4326)

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


# sfc interface
test_that('if coords null, geometry required', {
  expect_error(edge_direction(edges, DT, id = id, coords = NULL),
               'get_geometry?')
})

test_that('crs provided with geometry gives message crs ignored', {
  get_geometry(DT, coords = coords, crs = utm)
  expect_message(edge_direction(edges, DT, id = id, crs = utm),
                 'ignored')
})

test_that('geometry correctly provided else error', {
  expect_error(edge_direction(edges, DT, id = id, geometry = 'potato'),
               'is not present')
  expect_error(edge_direction(edges, DT, id = id, geometry = 'X'),
               'must be of class')
})

test_that('sfc interface message before overwrite', {
  copyEdges <- copy(edges)[, direction_dyad := 42]
  expect_message(edge_direction(copyEdges, DT, id = id),
                 'overwritten')
})

test_that('sfc interface returns expected', {
  copyEdges <- copy(edges)
  get_geometry(DT, coords = coords, crs = utm)

  expect_equal(
    ncol(copyEdges) + 1,
    ncol(edge_direction(copyEdges, DT, id = id))
  )

  expect_equal(
    nrow(copyEdges),
    nrow(edge_direction(copyEdges, DT, id = id))
  )

  expect_true('direction_dyad' %in% colnames(edge_direction(copyEdges, DT, id = id)))

  direction_step(DT, id = id)
  expect_true('direction' %in% colnames(edge_direction(copyEdges, DT, id = id)))

  outEdges <- edge_direction(copyEdges, DT, id = id)
  expect_type(outEdges$direction_dyad, 'double')
  expect_s3_class(outEdges$direction_dyad, 'units')

  expect_equal(max(outEdges$direction_dyad, na.rm = TRUE), units::as_units(pi, 'rad'),
               tolerance = 0.01)
  expect_equal(min(outEdges$direction_dyad, na.rm = TRUE), units::as_units(-pi, 'rad'),
               tolerance = 0.01)


})

