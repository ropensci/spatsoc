# Test group_pts
context('test group_pts')

library(spatsoc)

DT <- fread('../testdata/DT.csv')
id <- 'ID'
coords <- c('X', 'Y')
threshold <- 10
timegroup <- 'timegroup'


DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = 'datetime', threshold = '20 minutes')

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

  expect_error(
    group_pts(
      DT,
      threshold = threshold,
      id = id,
      coords = 'X',
      timegroup = timegroup
    ),
    'coords must be length 2',
    fixed = FALSE
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
  copyDT <- copy(DT)

  expect_error(
    group_pts(DT,
      threshold = -10, id = id,
      coords = coords,
      timegroup = timegroup
    ),
    'threshold must be > 0'
  )

  expect_error(
    group_pts(DT,
      threshold = 0, id = id,
      coords = coords,
      timegroup = timegroup
    ),
    'threshold must be > 0'
  )

  expect_error(
    group_pts(DT,
      threshold = '0', id = id,
      coords = coords,
      timegroup = timegroup
    ),
    'threshold must be of class numeric'
  )
})


test_that('coords are correctly provided or error detected', {
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
})

test_that('DT returned if timegroup, group fields not provided', {
  copyDT <- copy(DT)
  expect_equal(
    ncol(copyDT) + 1,
    ncol(group_pts(
      copyDT,
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = timegroup
    ))
  )

  # warns if > 1 ID row

  # same but with timegroup

  # and with splitBy
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
})


test_that('withinGroup is not returned to the user', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '5 minutes')

  expect_false('withinGroup' %in% colnames(
    group_pts(
      copyDT,
      threshold = threshold,
      id = id,
      coords = coords,
      timegroup = timegroup
    )
  ))
})

test_that('no rows are added to the result DT', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '5 minutes')

  expect_equal(
    nrow(copyDT),
    nrow(
      group_pts(
        copyDT,
        threshold = threshold,
        id = id,
        coords = coords,
        timegroup = timegroup
      )
    )
  )
})

test_that('only one column added to the result DT', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '5 minutes')

  expect_equal(
    ncol(copyDT) + 1,
    ncol(
      group_pts(
        copyDT,
        threshold = threshold,
        id = id,
        coords = coords,
        timegroup = timegroup
      )
    )
  )
})

test_that('group column is added to result', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '5 minutes')
  expect_true('group' %in%
    colnames(
      group_pts(
        copyDT,
        threshold = threshold,
        id = id,
        coords = coords,
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
  expect_s3_class(group_pts(
    DT,
    threshold = threshold,
    id = id,
    coords = coords,
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
})
