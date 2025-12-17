# Test edge_dist
context('test edge_dist')

library(spatsoc)

coords <- c('X', 'Y')
id <- 'ID'
utm <- 32736
threshold <- 10
timegroup <- 'timegroup'

DT <- fread('../testdata/DT.csv')
group_times(DT, 'datetime', '10 minutes')

get_geometry(DT, coords = coords, crs = 32736)

test_that('error/warn/msg if args are not provided as expected', {
  expect_error(
    edge_dist(
      DT = NULL
    ),
    'DT must be provided'
  )

  expect_error(
    edge_dist(
      DT = data.frame()
    ),
    'data.table'
  )

  expect_error(
    edge_dist(DT),
    'threshold'
  )

  expect_error(
    edge_dist(DT, threshold = threshold, id = NULL),
    'id'
  )

  expect_error(
    edge_dist(DT, threshold = threshold, id = id),
    'timegroup'
  )

  expect_error(
    edge_dist(DT, threshold = threshold, id = id, timegroup = NULL),
    'timegroup'
  )

  # geometry
  expect_message(
    edge_dist(DT, threshold = threshold, id = id, timegroup = timegroup, crs = utm),
    'crs argument is ignored'
  )
})


test_that('column names must exist in DT', {
  expect_error(
    edge_dist(
      DT,
      threshold = threshold,
      id = 'potato',
      coords = coords,
      timegroup = timegroup
    ),
    'not present in input'
  )

  expect_error(
    edge_dist(
      DT,
      threshold = threshold,
      id = id,
      coords = c('potatoX', 'potatoY'),
      timegroup = timegroup
    ),
    'not present in input'
  )

  expect_error(
    edge_dist(
      DT,
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = timegroup,
      splitBy = 'potato'
    ),
    'not present in input'
  )

  expect_error(
    edge_dist(
      DT,
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = 'potato'
    ),
    'not present in input'
  )
})

test_that('warns if timegroup is a datetime or character', {
  copyDT <- copy(DT)
  expect_warning(
    edge_dist(
      copyDT,
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = 'datetime'
    ),
    'timegroup provided is a',
    fixed = FALSE
  )

  copyDT <- copy(DT)
  copyDT[, posix := as.POSIXct(datetime)]
  expect_warning(
    edge_dist(
      copyDT,
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = 'posix'
    ),
    'timegroup provided is a',
    fixed = FALSE
  )

  copyDT <- copy(DT)
  copyDT[, idate := as.IDate(datetime)]
  expect_warning(
    edge_dist(
      copyDT,
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = 'idate'
    ),
    'timegroup provided is a',
    fixed = FALSE
  )
})

test_that('duplicate IDs in a timegroup detected', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '8 hours')
  expect_warning(
    edge_dist(
      copyDT,
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = timegroup
    ),
    'found duplicate id in a timegroup',
    fixed = FALSE
  )
})


test_that('warns about splitBy column', {
  copyDT <- copy(DT)

  group_times(copyDT, 'datetime', '5 minutes')
  copyDT[, splitBy := as.IDate(datetime)]

  expect_warning(
    edge_dist(
      copyDT,
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = timegroup
    ),
    'split_by'
  )
})

test_that('threshold correctly provided or error detected', {
  expect_error(
    edge_dist(DT,
      threshold = -10, timegroup = timegroup, id = id,
      coords = coords
    ),
    'threshold must be > 0'
  )

  expect_error(
    edge_dist(DT,
      threshold = 0, timegroup = timegroup, id = id,
      coords = coords
    ),
    'threshold must be > 0'
  )

  expect_error(
    edge_dist(DT,
      threshold = '0', timegroup = timegroup, id = id,
      coords = coords
    ),
    'threshold must be of class numeric'
  )

  expect_error(
    edge_dist(DT,
              threshold = -10, timegroup = timegroup, id = id,
              coords = coords, crs = utm
    ),
    'threshold must be > 0'
  )

  expect_error(
    edge_dist(DT,
              threshold = 0, timegroup = timegroup, id = id,
              coords = coords, crs = utm
    ),
    'threshold must be > 0'
  )

  expect_error(
    edge_dist(DT,
              threshold = '0', timegroup = timegroup, id = id,
              coords = coords, crs = utm
    ),
    'threshold must be of class numeric'
  )

  expect_error(
    edge_dist(DT,
              threshold = units::as_units(-1, 'm'),
              timegroup = timegroup, id = id,
              coords = coords, crs = utm
    ),
    'threshold must be > units'
  )

  # geometry
  expect_error(
    edge_dist(DT,
              threshold = units::as_units(-1, 'm'),
              timegroup = timegroup, id = id
    ),
    'threshold must be > units'
  )

  expect_error(
    edge_dist(DT,
              threshold = units::as_units(100, 'km'),
              timegroup = timegroup, id = id
    ),
    'units of threshold'
  )

})


test_that('error coords/geometry are not provided as expected', {
  # coords
  expect_error(
    edge_dist(
      DT,
      threshold = threshold,
      id = id,
      coords = c('X', NULL),
      timegroup = timegroup
    ),
    'coords must be length 2'
  )

  expect_error(
    edge_dist(
      DT,
      threshold = threshold,
      id = id,
      coords = c('X', 'ID'),
      timegroup = timegroup
    ),
    'coords must be of class numeric'
  )

  # geometry
  expect_error(
    edge_dist(
      DT,
      threshold = threshold,
      id = id,
      timegroup = timegroup,
      geometry = 'potato'
    ),
    'did you run'
  )

  expect_error(
    edge_dist(
      DT,
      threshold = threshold,
      id = id,
      timegroup = timegroup,
      geometry = 'X'
    ),
    'sfc_POINT'
  )
})


test_that('returned IDs make sense', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '10 minutes')
  eDT <- edge_dist(
    copyDT,
    threshold = threshold,
    id = id,
    coords = coords,
    timegroup = timegroup,
    fillNA = TRUE
  )

  IDs <- copyDT[, unique(ID)]
  expect_true(all(eDT$ID1 %in% IDs))
  expect_true(all(na.omit(eDT$ID2) %in% IDs))
  expect_true(eDT[ID1 == ID2, .N] == 0)

  eDT <- edge_dist(
    copyDT,
    threshold = threshold,
    id = id,
    coords = coords,
    timegroup = timegroup,
    fillNA = FALSE
  )

  IDs <- copyDT[, unique(ID)]
  expect_true(all(eDT$ID1 %in% IDs))
  expect_true(all(eDT$ID2 %in% IDs))
  expect_true(eDT[ID1 == ID2, .N] == 0)
})



test_that('returnDist works', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '10 minutes')

  withDist <- edge_dist(
    copyDT,
    threshold = threshold,
    id = id,
    coords = coords,
    timegroup = timegroup,
    returnDist = TRUE,
    fillNA = TRUE
  )

  woDist <- edge_dist(
    copyDT,
    threshold = threshold,
    id = id,
    coords = coords,
    timegroup = timegroup,
    returnDist = FALSE,
    fillNA = TRUE
  )

  woThresh <- edge_dist(
    copyDT,
    threshold = NULL,
    id = id,
    coords = coords,
    timegroup = timegroup,
    returnDist = TRUE,
    fillNA = TRUE
  )

  expect_equal(
    withDist[, .(ID1, ID2, timegroup)],
    woDist[, .(ID1, ID2, timegroup)]
  )

  expect_equal(
    nrow(withDist),
    nrow(woDist)
  )

  expect_equal(
    withDist[is.na(ID2)],
    withDist[is.na(distance)]
  )

  expect_equal(
    withDist[!is.na(ID2)],
    withDist[!is.na(distance)]
  )

  expect_lt(
    withDist[, max(distance, na.rm = TRUE)],
    threshold
  )

  expect_gt(woThresh[, .N], withDist[, .N])
  expect_gt(woThresh[distance > threshold, .N], 0)

  withDistNoNA <- edge_dist(
    copyDT,
    threshold = threshold,
    id = id,
    coords = coords,
    timegroup = timegroup,
    returnDist = TRUE,
    fillNA = FALSE
  )

  expect_true(withDistNoNA[is.na(distance), .N] == 0)
  expect_true(withDistNoNA[is.na(ID2), .N] == 0)
  expect_lt(
    withDistNoNA[, max(distance, na.rm = TRUE)],
    threshold
  )
})


test_that('returns a data.table', {
  expect_s3_class(edge_dist(
    DT,
    threshold = threshold,
    id = id,
    coords = coords,
    timegroup = timegroup
  ), 'data.table')
})



test_that('warns about splitBy column', {
  copyDT <- copy(DT)

  group_times(copyDT, 'datetime', '5 minutes')
  copyDT[, splitBy := as.IDate(datetime)]

  expect_warning(
    edge_dist(
      copyDT,
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = timegroup
    ),
    'split_by'
  )
})


test_that('handles NULL threshold', {
  expect_equal(
    edge_dist(
      DT,
      threshold = NULL,
      id = id,
      coords = coords,
      timegroup = timegroup
    ),
    edge_dist(
      DT,
      threshold = Inf,
      id = id,
      coords = coords,
      timegroup = timegroup
    )
  )
})

test_that('errors if timegroup is null', {
  expect_error(
    edge_dist(
      DT,
      threshold = NULL,
      id = id,
      coords = coords,
      timegroup = NULL
    )
  )
})
