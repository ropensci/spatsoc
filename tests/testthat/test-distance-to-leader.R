# Test distance_to_leader
context('test distance_to_leader')

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
group_times(DT, datetime = datetime, timethreshold)
group_pts(DT, threshold = threshold, id = id,
          coords = coords, timegroup = timegroup)
centroid_group(DT, coords = coords, group = group)
direction_step(DT = DT, id = id, coords = coords, crs = utm)
direction_group(DT)
leader_direction_group(DT, coords = coords, group = group, crs = utm,
                       return_rank = TRUE)

get_geometry(DT, coords = coords, crs = utm)

# Removing group with missing leader
DT_with_missing <- copy(DT)
DT <- copy(DT)[
  !group %in%
    DT[, any(rank_position_group_direction == 1, na.rm = TRUE), by = group][
      !(V1), group]][group != 868]

clean_DT <- copy(DT)

test_that('arguments required, otherwise error detected', {
  expect_error(distance_to_leader(DT = NULL))

  expect_error(distance_to_leader(DT, coords = coords, group = NULL),
               'group must be provided')
})

test_that('column names must exist in DT', {
  expect_error(distance_to_leader(DT, coords = rep('potato', 2), group = group),
               'potato field')
  expect_error(distance_to_leader(DT, coords = coords, group = 'potato'),
               'potato field')
  copy_DT <- copy(DT)
  setnames(copy_DT, 'rank_position_group_direction', 'potato')
  expect_error(distance_to_leader(copy_DT, coords = coords, group = group),
               'did you run leader?')

  copy_DT <- copy(DT)[, rank_position_group_direction := NULL]
  expect_error(distance_to_leader(copy_DT, coords = coords,
                                  group = group))
})

test_that('coords/geometry are correctly provided or error detected', {
  expect_error(distance_to_leader(DT, coords = c('X', NULL), group = group),
               'coords must be length 2')
  copy_DT <- copy(DT)[, X := as.character(X)]
  expect_error(distance_to_leader(copy_DT, coords = coords, group = group),
               'coords must be of class numeric')
  copy_DT <- copy(DT)[, X := as.character(X)]
  expect_error(distance_to_leader(copy_DT, coords = coords,
                                  group = group),
               'coords must be of class numeric')

  # geometry
  expect_error(distance_to_leader(DT, geometry = 'potato'),
               'get_geometry')
  expect_message(distance_to_leader(DT, crs = utm),
                 'crs argument is ignored')
  expect_error(distance_to_leader(DT, geometry = 'X'))
})

test_that('leader is correctly provided or error detected', {
  copy_DT <- copy(DT)[, rank_position_group_direction :=
                        as.character(rank_position_group_direction)]
  expect_error(distance_to_leader(copy_DT, coords = coords, group = group),
               'must be of class numeric')
})

test_that('warns if group does not have a leader', {
  expect_warning(
    distance_to_leader(
      DT = DT_with_missing,
      coords = coords,
      group = 'group'
    ),
    'missing leader'
  )
})

test_that('message when distance_leader column overwritten', {
  # coords
  copyDT <- copy(clean_DT)[, distance_leader := 1]
  expect_message(
    distance_to_leader(copyDT, coords = coords, group = group),
    'distance_leader column will be overwritten'
  )

  # geometry
  copyDT <- copy(clean_DT)[, distance_leader := 1]
  expect_message(
    distance_to_leader(copyDT, group = group),
    'distance_leader column will be overwritten'
  )
})

test_that('no rows are added to the result DT', {
  # coords
  copyDT <- copy(clean_DT)
  expect_equal(nrow(copyDT),
               nrow(distance_to_leader(copyDT, coords = coords, group = group)))

  # geometry
  copyDT <- copy(clean_DT)
  expect_equal(nrow(copyDT),
               nrow(distance_to_leader(copyDT, group = group)))
})

test_that('one column added to the result DT', {
  # coords
  copyDT <- copy(clean_DT)
  expect_equal(ncol(clean_DT) + 1,
               ncol(distance_to_leader(copyDT, coords = coords, group = group)))

  # And check modifies by reference
  copyDT <- copy(clean_DT)
  distance_to_leader(copyDT, coords = coords, group = group)
  expect_equal(ncol(clean_DT) + 1, ncol(copyDT))

  # geometry
  copyDT <- copy(clean_DT)
  expect_equal(ncol(clean_DT) + 1,
               ncol(distance_to_leader(copyDT, group = group)))

  # And check modifies by reference
  copyDT <- copy(clean_DT)
  distance_to_leader(copyDT, group = group)
  expect_equal(ncol(clean_DT) + 1, ncol(copyDT))
})

test_that('column added to the result DT is a double', {
  # coords
  expect_type(
    distance_to_leader(DT, coords = coords, group = group)$distance_leader,
    'double'
  )

  # geometry
  expect_type(
    distance_to_leader(DT, group = group)$distance_leader,
    'double'
  )
})

test_that('zzz columns not added to the result', {
  # coords
  zzz_cols <- c('has_leader', 'zzz_N_by_group')
  expect_false(
    any(zzz_cols %in% colnames(direction_to_leader(DT, coords = coords, crs = utm)))
  )

  # geometry
  zzz_cols <- c('has_leader', 'zzz_N_by_group', 'zzz_geometry_leader')
  expect_false(
    any(zzz_cols %in% colnames(direction_to_leader(DT, crs = utm)))
  )
})

test_that('returns a data.table', {
  # coords
  expect_s3_class(distance_to_leader(DT, coords = coords, group = group),
                  'data.table')

  # geometry
  expect_s3_class(distance_to_leader(DT, group = group),
                  'data.table')
})


expect_DT <- data.table(
  ID = c('A', 'B'),
  X = c(0, 10),
  Y = c(0, 0),
  group_direction = rep(units::as_units(0, 'rad'), 2),
  group = c(1, 1)
)
centroid_group(expect_DT, coords = coords)
leader_direction_group(expect_DT, coords = coords, crs = utm,
                       return_rank = TRUE, group = group)
distance_to_leader(expect_DT, coords = c('X', 'Y'))

test_that('expected results for simple case', {
  expect_lte(
    expect_DT[, max(distance_leader)],
    10
  )

  expect_gte(
    expect_DT[, min(distance_leader)],
    0
  )
  expect_equal(
    expect_DT[distance_leader == min(distance_leader), ID],
    'B'
  )
})

# geometry
expect_DT <- data.table(
  ID = c('A', 'B'),
  X = c(0, 10),
  Y = c(0, 0),
  group_direction = rep(units::as_units(0, 'rad'), 2),
  group = c(1, 1)
)
centroid_group(expect_DT, coords = coords)
leader_direction_group(expect_DT, coords = coords, crs = utm,
                       return_rank = TRUE, group = group)
get_geometry(expect_DT, coords = coords, crs = NA)
distance_to_leader(expect_DT)

test_that('expected results for simple case', {
  expect_lte(
    expect_DT[, max(distance_leader)],
    10
  )

  expect_gte(
    expect_DT[, min(distance_leader)],
    0
  )
  expect_equal(
    expect_DT[distance_leader == min(distance_leader), ID],
    'B'
  )
})

