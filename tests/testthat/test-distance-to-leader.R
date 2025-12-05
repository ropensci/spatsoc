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
centroid_group(DT, coords = coords, group = group, na.rm = TRUE)
direction_step(DT = DT, id = id, coords = coords, crs = utm)
direction_group(DT)
leader_direction_group(DT, coords = coords, group = group, return_rank = TRUE)

# Removing group with missing leader
DT <- copy(DT)[group != 868]

clean_DT <- copy(DT)

test_that('DT is required', {
  expect_error(distance_to_leader(DT = NULL))
})

test_that('arguments required, otherwise error detected', {
  expect_error(distance_to_leader(DT, coords = NULL, group = group),
               'geometry field')
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
})

test_that('coords are correctly provided or error detected', {
  expect_error(distance_to_leader(DT, coords = c('X', NULL), group = group),
               'coords must be length 2')
  copy_DT <- copy(DT)[, X := as.character(X)]
  expect_error(distance_to_leader(copy_DT, coords = coords, group = group),
               'coords must be of class numeric')
  copy_DT <- copy(DT)[, X := as.character(X)]
  expect_error(distance_to_leader(copy_DT, coords = coords,
                                  group = group),
               'coords must be of class numeric')

  copy_DT <- copy(DT)[, rank_position_group_direction := NULL]
  expect_error(distance_to_leader(copy_DT, coords = coords,
                                  group = group))
})

test_that('leader is correctly provided or error detected', {
  copy_DT <- copy(DT)[, rank_position_group_direction :=
                        as.character(rank_position_group_direction)]
  expect_error(distance_to_leader(copy_DT, coords = coords, group = group),
               'must be of class numeric')
})

test_that('message when distance_leader column overwritten', {
  copyDT <- copy(clean_DT)[, distance_leader := 1]
  expect_message(
    distance_to_leader(copyDT, coords = coords, group = group),
    'distance_leader column will be overwritten'
  )
})

test_that('no rows are added to the result DT', {
  copyDT <- copy(clean_DT)

  expect_equal(nrow(copyDT),
               nrow(distance_to_leader(copyDT, coords = coords, group = group)))
})

test_that('one column added to the result DT', {
  copyDT <- copy(clean_DT)

  expect_equal(ncol(clean_DT) + 1,
               ncol(distance_to_leader(copyDT, coords = coords, group = group)))

  # And check modifies by reference
  copyDT <- copy(clean_DT)
  distance_to_leader(copyDT, coords = coords, group = group)
  expect_equal(ncol(clean_DT) + 1, ncol(copyDT))
})

test_that('column added to the result DT is a double', {
  expect_type(
    distance_to_leader(DT, coords = coords, group = group)$distance_leader,
    'double'
  )
})

test_that('zzz columns not added to the result', {
  zzz_cols <- c('has_leader', 'zzz_N_by_group')

  expect_false(
    any(zzz_cols %in% colnames(direction_to_leader(DT, coords = coords)))
  )
})

test_that('returns a data.table', {
  expect_s3_class(distance_to_leader(DT, coords = coords, group = group),
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
leader_direction_group(expect_DT, coords = coords,
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



test_that('warns if group does not have a leader', {
  leaderless <- copy(DT)[group %in% DT[, .N, group][N > 1, group]]
  leaderless[,
    rank_position_group_direction := fifelse(
      rank_position_group_direction == 1, 0, rank_position_group_direction
    )
  ]
  leaderless[, .(
    has_leader = any(rank_position_group_direction == 1)),
    by = c(group)][!(has_leader)]
  expect_warning(
    distance_to_leader(
      DT = leaderless,
      coords = coords,
      group = 'group'
    ),
    'missing leader'
  )
})
