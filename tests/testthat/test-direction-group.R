# Test direction_group
context('test direction_group')

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
group_pts(DT, threshold = threshold, id = id, coords = coords, timegroup = timegroup)
direction_step(DT, id = id, coords = coords, projection = projection)

clean_DT <- copy(DT)

test_that('DT is required', {
  expect_error(direction_group(DT = NULL))
})

test_that('arguments required, otherwise error detected', {
  expect_error(direction_group(DT, group = NULL),
               'group column name required')
  expect_error(direction_group(DT, direction = NULL),
               'direction column name required')
})

test_that('column names must exist in DT', {
  expect_error(direction_group(DT, direction = 'potato'),
               'potato field')
  expect_error(direction_group(DT, group = 'potato'),
               'potato field')
})

test_that('radians expected else error', {
  expect_error(direction_group(DT, direction = 'X'),
               'direction_step')
})


test_that('group_direction column succesfully detected', {
  copyDT <- copy(clean_DT)[, group_direction := 1]
  expect_message(
    direction_group(copyDT),
    'group_direction column will be overwritten'
  )
})

test_that('no rows are added to the result DT', {
  copyDT <- copy(clean_DT)

  expect_equal(nrow(copyDT),
               nrow(direction_group(copyDT)))
})

test_that('one column added to the result DT', {
  copyDT <- copy(clean_DT)

  expect_equal(ncol(copyDT) + 1,
               ncol(direction_group(DT)))
})

test_that('column added to the result DT are radians', {
  expect_type(direction_group(DT)$group_direction, 'double')
  expect_equal(units(direction_group(DT)$group_direction)$numerator, 'rad')
})

test_that('returns a data.table', {
  expect_s3_class(direction_group(DT), 'data.table')
})
