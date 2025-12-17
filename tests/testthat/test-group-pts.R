# Test group_pts
context('test group_pts')

library(spatsoc)

DT <- fread('../testdata/DT.csv')
id <- 'ID'
coords <- c('X', 'Y')
threshold <- 10
timegroup <- 'timegroup'
utm <- 32736


DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = 'datetime', threshold = '20 minutes')
get_geometry(DT, coords, utm)
get_geometry(DT, coords, utm, 4326, 'geometry_longlat')

test_that('args provided as expected else conditions', {
  expect_error(
    group_pts(
      DT = NULL,
      threshold = threshold,
      id = id
    ),
    'DT must be provided'
  )
  expect_error(
    group_pts(DT, threshold = threshold, id = NULL),
    'id must be'
  )

  expect_error(
    group_pts(DT, threshold = NULL, id = id),
    'threshold must be'
  )

  expect_error(
    group_pts(DT,
      threshold = threshold, id = id,
      coords = coords
    ),
    'timegroup must be'
  )
})

test_that('column names must exist in DT', {
  expect_error(
    group_pts(
      DT,
      threshold = threshold,
      id = 'potato',
      coords = coords,
      timegroup = timegroup
    ),
    'not present in input',
    fixed = FALSE
  )

  expect_error(
    group_pts(
      DT,
      threshold = threshold,
      id = id,
      coords = c('potatoX', 'potatoY'),
      timegroup = timegroup
    ),
    'not present in input',
    fixed = FALSE
  )

  expect_error(
    group_pts(
      DT,
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = timegroup,
      splitBy = 'potato'
    ),
    'not present in input',
    fixed = FALSE
  )

  expect_error(
    group_pts(
      DT,
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = 'potato'
    ),
    'not present in input',
    fixed = FALSE
  )
})


test_that('threshold correctly provided or error detected', {
  expect_error(
    group_pts(DT,
            threshold = -10, timegroup = timegroup, id = id,
            coords = coords
    ),
    'threshold must be > 0'
  )

  expect_error(
    group_pts(DT,
            threshold = 0, timegroup = timegroup, id = id,
            coords = coords
    ),
    'threshold must be > 0'
  )

  expect_error(
    group_pts(DT,
            threshold = '0', timegroup = timegroup, id = id,
            coords = coords
    ),
    'threshold must be of class numeric'
  )

  expect_error(
    group_pts(DT,
            threshold = -10, timegroup = timegroup, id = id,
            coords = coords, crs = utm
    ),
    'threshold must be > 0'
  )

  expect_error(
    group_pts(DT,
            threshold = 0, timegroup = timegroup, id = id,
            coords = coords, crs = utm
    ),
    'threshold must be > 0'
  )

  expect_error(
    group_pts(DT,
            threshold = '0', timegroup = timegroup, id = id,
            coords = coords, crs = utm
    ),
    'threshold must be of class numeric'
  )

  expect_error(
    group_pts(DT,
            threshold = units::as_units(-1, 'm'),
            timegroup = timegroup, id = id,
            coords = coords, crs = utm
    ),
    'threshold must be > 0'
  )

  # geometry
  expect_error(
    group_pts(DT,
            threshold = units::as_units(-1, 'm'),
            timegroup = timegroup, id = id
    ),
    'threshold must be > 0'
  )

  expect_error(
    group_pts(DT,
            threshold = units::as_units(100, 'km'),
            timegroup = timegroup, id = id
    ),
    'units of threshold'
  )
})


test_that('coords/geometry are correctly provided or error detected', {
  # coords
  expect_error(
    group_pts(
      DT,
      threshold = threshold,
      id = id,
      coords = c('X', NULL),
      timegroup = timegroup
    ),
    'coords must be length 2'
  )

  expect_error(
    group_pts(
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
    group_pts(
      DT,
      threshold = threshold,
      id = id,
      timegroup = timegroup,
      geometry = 'potato'
    ),
    'did you run'
  )

  expect_error(
    group_pts(
      DT,
      threshold = threshold,
      id = id,
      timegroup = timegroup,
      geometry = 'X'
    ),
    'sfc_POINT'
  )
})

test_that('warns if timegroup is a datetime or character', {
  copyDT <- copy(DT)
  expect_warning(
    group_pts(
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
    group_pts(
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
    group_pts(
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


test_that('group column succesfully detected', {
  # coords
  copyDT <- copy(DT)[, group := 1]
  expect_message(
    group_pts(
      copyDT,
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = timegroup
    ),
    'group column will be overwritten'
  )

  # geometry
  copyDT <- copy(DT)[, group := 1]
  expect_message(
    group_pts(
      copyDT,
      threshold = threshold,
      id = id,
      timegroup = timegroup
    ),
    'group column will be overwritten'
  )
})


test_that('withinGroup is not returned to the user', {
  # coords
  expect_false('withinGroup' %in% colnames(
    group_pts(
      copy(DT),
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = timegroup
    )
  ))

  # geometry
  expect_false('withinGroup' %in% colnames(
    group_pts(
      copy(DT),
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = timegroup
    )
  ))
})

test_that('no rows are added to the result DT', {
  # coords
  expect_equal(
    nrow(copy(DT)),
    nrow(
      group_pts(
        copy(DT),
        threshold = threshold,
        id = id,
        coords = coords,
        timegroup = timegroup
      )
    )
  )

  # geometry
  expect_equal(
    nrow(copy(DT)),
    nrow(
      group_pts(
        copy(DT),
        threshold = threshold,
        id = id,
        timegroup = timegroup
      )
    )
  )
})

test_that('group column added to the result DT', {
  # coords
  expect_equal(
    ncol(copy(DT)) + 1,
    ncol(
      group_pts(
        copy(DT),
        threshold = threshold,
        id = id,
        coords = coords,
        timegroup = timegroup
      )
    )
  )

  expect_true('group' %in%
    colnames(
      group_pts(
        copy(DT),
        threshold = threshold,
        id = id,
        coords = coords,
        timegroup = timegroup
      )
    ))

  # geometry
  expect_equal(
    ncol(copy(DT)) + 1,
    ncol(
      group_pts(
        copy(DT),
        threshold = threshold,
        id = id,
        timegroup = timegroup
      )
    )
  )

  expect_true('group' %in%
    colnames(
      group_pts(
        copy(DT),
        threshold = threshold,
        id = id,
        timegroup = timegroup
      )
    ))
})

test_that('duplicate IDs in a timegroup detected', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '8 hours')
  expect_warning(
    group_pts(
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

test_that('returns a data.table', {
  # coords
  expect_s3_class(group_pts(
    copy(DT),
    threshold = threshold,
    id = id,
    coords = coords,
    timegroup = timegroup
  ), 'data.table')

  # geometry
  expect_s3_class(group_pts(
    copy(DT),
    threshold = threshold,
    id = id,
    timegroup = timegroup
  ), 'data.table')
})


test_that('splitBy argument doesnt use splitBy column', {
  copyDT <- copy(DT)

  copyDT[, splitBy := sample(seq.int(5), .N, TRUE)]

  expect_true(
    group_pts(
      copyDT,
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = timegroup
    )[, uniqueN(splitBy), group][V1 > 1, .N != 0]
  )
})


test_that('group_pts returns NA for group when X/Y are NA', {
  # coords
  copyDT <- copy(DT)
  n <- 10
  copyDT[sample(.N, n), X := NA]

  expect_equal(
    group_pts(
      copyDT,
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = timegroup
    )[is.na(group), .N],
    n
  )

  copyDT <- copy(DT)
  n <- 10
  copyDT[sample(.N, n), Y := NA]

  expect_equal(
    group_pts(
      copyDT,
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = timegroup
    )[is.na(group), .N],
    n
  )

  copyDT <- copy(DT)
  n <- 10
  copyDT[sample(.N, n), c('X', 'Y') := NA]

  expect_equal(
    group_pts(
      copyDT,
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = timegroup
    )[is.na(group), .N],
    n
  )

  # geometry
  copyDT <- copy(DT)
  n <- 10
  copyDT[sample(.N, n), geometry := st_sfc(st_point())]

  expect_equal(
    group_pts(
      copyDT,
      threshold = threshold,
      id = id,
      timegroup = timegroup
    )[is.na(group), .N],
    n
  )

  # longlat
  copyDT <- copy(DT)
  n <- 10
  copyDT[sample(.N, n), geometry_longlat := st_sfc(st_point())]

  expect_equal(
    group_pts(
      copyDT,
      threshold = threshold,
      id = id,
      timegroup = timegroup,
      geometry = 'geometry_longlat'
    )[is.na(group), .N],
    n
  )
})
