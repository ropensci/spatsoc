# Test leader_direction_group
context('test leader_direction_group')

library(spatsoc)
library(units)

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
group_times(DT, datetime = datetime, timethreshold)
group_pts(DT, threshold = threshold, id = id,
          coords = coords, timegroup = timegroup)
centroid_group(DT, coords = coords, group = group)
direction_step(DT = DT, id = id, coords = coords, crs = utm)
direction_group(DT)

clean_DT <- copy(DT)

test_that('DT is required', {
  expect_error(leader_direction_group(DT = NULL))
})

test_that('arguments required, otherwise error detected', {
  expect_error(leader_direction_group(DT, coords = NULL),
               'coords ')
  expect_error(leader_direction_group(DT, coords = coords, return_rank = NULL),
               'return_rank must be')
  expect_error(leader_direction_group(DT, coords = coords, return_rank = TRUE,
                                      group = NULL),
               'group must be')
})

test_that('column names must exist in DT', {
  expect_error(leader_direction_group(DT, coords = rep('potato', 2)),
               'potato field')
  copy_DT <- copy(clean_DT)
  setnames(copy_DT, 'centroid_X', 'potato_X')
  expect_error(leader_direction_group(copy_DT, coords = coords),
               'centroid_X field')
  expect_error(leader_direction_group(DT, coords = coords, return_rank = TRUE,
                                      group = 'potato'),
               'group_pts')
})


test_that('coords are correctly provided or error detected', {
  expect_error(leader_direction_group(DT, coords = c('X', NULL)),
               'coords must be length 2')
  copy_DT <- copy(clean_DT)[, X := as.character(X)]
  expect_error(leader_direction_group(copy_DT, coords = coords),
               'coords must be of class numeric')
  copy_DT <- copy(clean_DT)[, centroid_X := as.character(centroid_X)]
  expect_error(leader_direction_group(copy_DT, coords = coords),
               'centroid_xcol must be of class numeric')
  copy_DT <- copy(clean_DT)[, centroid_Y := as.character(centroid_X)]
  expect_error(leader_direction_group(copy_DT, coords = coords),
               'centroid_ycol must be of class numeric')
})

test_that('radians expected else error', {
  copyDT <- copy(DT)[, group_direction := units::set_units(group_direction, 'degree')]
  expect_error(leader_direction_group(copyDT, coords = coords),
               'direction_group')
})

test_that('position_group_direction column succesfully detected', {
  copy_DT <- copy(clean_DT)[, position_group_direction := 1]
  expect_message(
    leader_direction_group(copy_DT, coords = coords),
    'position_group_direction column will be overwritten'
  )
})

test_that('no rows are added to the result DT', {
  copy_DT <- copy(clean_DT)

  expect_equal(nrow(copy_DT),
               nrow(leader_direction_group(copy_DT, coords = coords)))
})

test_that('1 or 2 (return_rank = TRUE) column(s) added to the result DT', {
  copy_DT <- copy(clean_DT)

  expect_equal(ncol(copy_DT) + 1,
               ncol(leader_direction_group(copy_DT, coords = coords)))

  copy_DT <- copy(clean_DT)
  expect_equal(ncol(copy_DT) + 2,
               ncol(leader_direction_group(copy_DT, coords = coords,
                                           return_rank = TRUE,
                                           group = 'group')))
})

test_that('column(s) added to the result DT are expected type', {
  expect_type(
    leader_direction_group(DT, coords = coords)$position_group_direction,
    'double'
  )
  expect_type(
    leader_direction_group(
      DT,
      coords = coords,
      group = 'group',
      return_rank = TRUE)$rank_position_group_direction,
    'double'
  )

})

test_that('column(s) added to the result DT are expected range', {
  expect_gt(
    leader_direction_group(DT, coords = coords)[
      position_group_direction < 0, .N],
    0
  )

  expect_gt(
    leader_direction_group(DT, coords = coords)[
      position_group_direction > 0, .N],
    0
  )

  expect_equal(
    leader_direction_group(DT, coords = coords,
                           group = 'group', return_rank = TRUE)[
      rank_position_group_direction < 0, .N],
    0
  )

  expect_gt(
    leader_direction_group(DT, coords = coords,
                           group = 'group', return_rank = TRUE)[
      position_group_direction > 0, .N],
    0
  )
})

test_that('returns a data.table', {
  expect_s3_class(leader_direction_group(DT, coords = coords), 'data.table')
})



expect_DT <- data.table(
  ID = c('A', 'B'),
  X = c(0, 10),
  Y = c(0, 0),
  group_direction = rep(as_units(0, 'rad'), 2),
  group = c(1, 1)
)
centroid_group(expect_DT, coords = coords)
leader_direction_group(expect_DT, coords = coords,
                       return_rank = TRUE, group = group)

test_that('expected results for simple case', {
  expect_lte(
    expect_DT[, max(rank_position_group_direction)],
    2
  )

  expect_lte(
    expect_DT[, max(position_group_direction)],
    5
  )
  expect_gte(
    expect_DT[, min(position_group_direction)],
    -5
  )

  expect_equal(
    expect_DT[rank_position_group_direction == 1, ID],
    'B'
  )
})

