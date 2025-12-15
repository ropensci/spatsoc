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

DT_geo <- copy(DT)
get_geometry(DT_geo, coords = coords, crs = utm)
get_geometry(DT_geo, coords = coords, crs = utm, output_crs = 4326,
             geometry_colname = 'geometry_longlat')
centroid_group(DT_geo)
direction_step(DT_geo, id = id)
direction_group(DT_geo)

clean_DT <- copy(DT)
clean_DT_geo <- copy(DT_geo)

test_that('DT is required', {
  expect_error(leader_direction_group(DT = NULL))
})

test_that('arguments required, otherwise error detected', {
  expect_error(leader_direction_group(DT, coords = coords, return_rank = NULL,
                                      crs = utm),
               'return_rank must be')
  expect_error(leader_direction_group(DT, coords = coords, group = NULL,
                                      crs = utm),
               'group must be')

  expect_error(leader_direction_group(DT, coords = coords, group = NULL,
                                      crs = NULL),
               'crs must be')

  # geo
  copy_DT <- copy(clean_DT_geo)[, geometry := NULL]
  expect_error(leader_direction_group(copy_DT), 'geometry')

  expect_message(leader_direction_group(DT_geo, crs = 4326),
                 'ignored')
})

test_that('column names must exist in DT', {
  expect_error(leader_direction_group(DT, coords = rep('potato', 2)),
               'potato field')
  copy_DT <- copy(clean_DT)
  setnames(copy_DT, 'centroid_X', 'potato_X')
  expect_error(leader_direction_group(copy_DT, coords = coords, crs = utm),
               'centroid_X field')
  expect_error(leader_direction_group(DT, coords = coords, crs = utm,
                                      group = 'potato'),
               'group_pts')

  # geo
  expect_error(leader_direction_group(DT_geo, geometry = 'potato'),
               'potato')

})


test_that('coords are correctly provided or error detected', {
  expect_error(leader_direction_group(DT, coords = c('X', NULL), crs = utm),
               'coords must be length 2')
  copy_DT <- copy(clean_DT)[, X := as.character(X)]
  expect_error(leader_direction_group(copy_DT, coords = coords, crs = utm),
               'coords must be of class numeric')
  copy_DT <- copy(clean_DT)[, centroid_X := as.character(centroid_X)]
  expect_error(leader_direction_group(copy_DT, coords = coords, crs = utm),
               'coords_centroid must be of class numeric')
  copy_DT <- copy(clean_DT)[, centroid_Y := as.character(centroid_X)]
  expect_error(leader_direction_group(copy_DT, coords = coords, crs = utm),
               'coords_centroid must be of class numeric')

  # geo
  expect_error(leader_direction_group(DT_geo, geometry = 'X'),
               'geometry')
})

test_that('radians expected else error', {
  copyDT <- copy(DT)[, group_direction := units::set_units(group_direction, 'degree')]
  expect_error(leader_direction_group(copyDT, coords = coords, crs = utm),
               'direction_group')
})

test_that('position_group_direction column succesfully detected', {
  copy_DT <- copy(clean_DT)[, position_group_direction := 1]
  expect_message(
    leader_direction_group(copy_DT, coords = coords, crs = utm),
    'position_group_direction column will be overwritten'
  )

  # geo
  copy_DT <- copy(clean_DT_geo)[, position_group_direction := 1]
  expect_message(leader_direction_group(copy_DT),
                 'position_group_direction')
})

test_that('no rows are added to the result DT', {
  copy_DT <- copy(clean_DT)
  expect_equal(nrow(copy_DT),
               nrow(leader_direction_group(copy_DT, coords = coords, crs = utm)))

  # geo
  copy_DT <- copy(clean_DT_geo)
  expect_equal(nrow(copy_DT),
               nrow(leader_direction_group(copy_DT)))
})

test_that('1 or 2 (return_rank = TRUE) column(s) added to the result DT', {
  copy_DT <- copy(clean_DT)
  expect_equal(ncol(copy_DT) + 1,
               ncol(leader_direction_group(copy_DT, coords = coords, crs = utm,
                                           return_rank = FALSE)))

  copy_DT <- copy(clean_DT)
  expect_equal(ncol(copy_DT) + 2,
               ncol(leader_direction_group(copy_DT, coords = coords, crs = utm,
                                           return_rank = TRUE,
                                           group = 'group')))

  # geo
  copy_DT <- copy(clean_DT_geo)
  expect_equal(ncol(copy_DT) + 1,
               ncol(leader_direction_group(copy_DT, return_rank = FALSE)))

  copy_DT <- copy(clean_DT_geo)
  expect_equal(ncol(copy_DT) + 2,
               ncol(leader_direction_group(copy_DT,
                                           return_rank = TRUE,
                                           group = 'group')))
})

test_that('column(s) added to the result DT are expected type', {
  expect_type(
    leader_direction_group(DT, coords = coords, crs = utm)$position_group_direction,
    'double'
  )
  expect_type(
    leader_direction_group(
      DT,
      coords = coords,
      crs = utm,
      group = 'group',
      return_rank = TRUE)$rank_position_group_direction,
    'double'
  )

  # geo
  copy_DT <- copy(clean_DT_geo)
  expect_type(
    leader_direction_group(copy_DT)$position_group_direction,
    'double'
  )
  expect_type(
    leader_direction_group(
      copy_DT)$rank_position_group_direction,
    'double'
  )

})

test_that('column(s) added to the result DT are expected range', {
  expect_gt(
    leader_direction_group(DT, coords = coords, crs = utm)[
      position_group_direction < 0, .N],
    0
  )

  expect_gt(
    leader_direction_group(DT, coords = coords, crs = utm)[
      position_group_direction > 0, .N],
    0
  )

  expect_equal(
    leader_direction_group(DT, coords = coords, crs = utm,
                           group = 'group')[
      rank_position_group_direction < 0, .N],
    0
  )

  expect_gt(
    leader_direction_group(DT, coords = coords, crs = utm,
                           group = 'group')[
      position_group_direction > 0, .N],
    0
  )

  # geo
  copy_DT <- copy(clean_DT_geo)
  expect_gt(
    leader_direction_group(copy_DT)[
      position_group_direction < 0, .N],
    0
  )

  expect_gt(
    leader_direction_group(copy_DT)[
      position_group_direction > 0, .N],
    0
  )

  expect_equal(
    leader_direction_group(copy_DT)[
      rank_position_group_direction < 0, .N],
    0
  )

  expect_gt(
    leader_direction_group(copy_DT)[
      position_group_direction > 0, .N],
    0
  )
})

test_that('returns a data.table', {
  expect_s3_class(leader_direction_group(DT, coords = coords, crs = utm), 'data.table')
  expect_s3_class(leader_direction_group(DT_geo), 'data.table')
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
                       return_rank = TRUE, group = group,
                       crs = utm)

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

# geo
expect_DT_geo <- data.table(
  ID = c('A', 'B'),
  X = c(0, 10),
  Y = c(0, 0),
  group_direction = rep(as_units(0, 'rad'), 2),
  group = c(1, 1)
)
get_geometry(expect_DT_geo, coords = coords, crs = utm)
centroid_group(expect_DT_geo)
leader_direction_group(expect_DT_geo)

test_that('expected results for simple case', {
  expect_lte(
    expect_DT_geo[, max(rank_position_group_direction)],
    2
  )

  expect_lte(
    expect_DT_geo[, max(position_group_direction)],
    5
  )
  expect_gte(
    expect_DT_geo[, min(position_group_direction)],
    -5
  )

  expect_equal(
    expect_DT_geo[rank_position_group_direction == 1, ID],
    'B'
  )
})


test_that('ud_units must be m', {
  expect_error(leader_direction_group(DT, coords = coords, group = 'group',
                                      crs = 4326),
               'planar')
})
