# Test distance_to_centroid
context('test distance_to_centroid')

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

get_geometry(DT, coords = coords, crs = utm)
centroid_group(DT, group = group)

clean_DT <- copy(DT)

test_that('arguments as expected, otherwise error/message/warn', {
  expect_error(distance_to_centroid(DT = NULL))

  expect_error(distance_to_centroid(DT, coords = coords, group = NULL,
                                    return_rank = TRUE),
               'group must be provided')
  expect_error(distance_to_centroid(DT, coords = coords, group = group,
                                    return_rank = NULL),
               'return_rank')
})

test_that('column names must exist in DT', {
  expect_error(distance_to_centroid(DT, coords = rep('potato', 2)),
               'potato field')
  expect_error(distance_to_centroid(DT, coords = coords, group = 'potato',
                                    return_rank = TRUE),
               'potato field')
})

test_that('coords/geometry are correctly provided or error detected', {
  expect_error(distance_to_centroid(DT, coords = c('X', NULL)),
               'coords must be length 2')
  copy_DT <- copy(DT)[, X := as.character(X)]
  expect_error(distance_to_centroid(copy_DT, coords = coords),
               'coords must be of class numeric')
  copy_DT <- copy(DT)[, centroid_X := as.character(centroid_X)]
  expect_error(distance_to_centroid(copy_DT, coords = coords),
               'coords_centroid must be of class numeric')

  # geometry
  expect_error(distance_to_centroid(DT, geometry = 'potato'),
               'get_geometry')
  expect_message(distance_to_centroid(DT, crs = utm),
                 'crs argument is ignored')
  expect_error(distance_to_centroid(DT, geometry = 'X'))
})

test_that('distance_centroid column succesfully detected', {
  # coords
  copyDT <- copy(clean_DT)[, distance_centroid := 1]
  expect_message(
    distance_to_centroid(copyDT, coords = coords),
    'distance_centroid column will be overwritten'
  )

  # geometry
  copyDT <- copy(clean_DT)[, distance_centroid := 1]
  expect_message(
    distance_to_centroid(copyDT),
    'distance_centroid column will be overwritten'
  )
})

test_that('no rows are added to the result DT', {
  # coords
  copyDT <- copy(clean_DT)
  expect_equal(nrow(copyDT),
               nrow(distance_to_centroid(copyDT, coords = coords)))

  # geometry
  copyDT <- copy(clean_DT)
  expect_equal(nrow(copyDT),
               nrow(distance_to_centroid(copyDT)))
})

test_that('1/2 columns added to the result DT (depending on return_rank)', {
  # coords
  copyDT <- copy(clean_DT)
  expect_equal(ncol(copyDT) + 2,
               ncol(distance_to_centroid(copyDT, coords = coords,
                                         return_rank = TRUE)))

  copyDT <- copy(clean_DT)
  expect_equal(ncol(copyDT) + 1,
               ncol(distance_to_centroid(copyDT, coords = coords,
                                         return_rank = FALSE)))

  # geometry
  copyDT <- copy(clean_DT)
  expect_equal(ncol(copyDT) + 2,
               ncol(distance_to_centroid(copyDT, return_rank = TRUE)))

  copyDT <- copy(clean_DT)
  expect_equal(ncol(copyDT) + 1,
               ncol(distance_to_centroid(copyDT, return_rank = FALSE)))
})

test_that('column added to the result DT is a double', {
  # coords
  expect_type(distance_to_centroid(DT, coords = coords)$distance_centroid,
              'double')

  expect_type(distance_to_centroid(DT, coords = coords, crs = utm)$distance_centroid,
              'double')

  # geometry
  copyDT <- copy(clean_DT)
  get_geometry(copyDT, coords = coords, crs = utm, output_crs = 4326,
               geometry_colname = 'geometry_longlat')
  centroid_group(copyDT, group = group, geometry = 'geometry_longlat')
  expect_type(distance_to_centroid(copyDT)$distance_centroid, 'double')
})

test_that('returns a data.table', {
  # coords
  expect_s3_class(distance_to_centroid(DT, coords = coords), 'data.table')

  # geometry
  expect_s3_class(distance_to_centroid(DT), 'data.table')
})

test_that('message if overwritting rank_distance_centroid', {
  # coords
  copy_DT <- copy(clean_DT)
  copy_DT[, rank_distance_centroid := 'potato']

  expect_message(distance_to_centroid(copy_DT, coords = coords, group = group,
                                      return_rank = TRUE),
                 'rank_distance_centroid')

  # geometry
  copy_DT <- copy(clean_DT)
  copy_DT[, rank_distance_centroid := 'potato']

  expect_message(distance_to_centroid(copy_DT, group = group,
                                      return_rank = TRUE),
                 'rank_distance_centroid')
})

test_that('NA for centroid returns NA for distance', {
  # coords
  copy_DT <- copy(DT)[sample(.N, 1e3)]
  copy_DT[sample(.N, 1e2), X := NA]
  expect_all_true(
    distance_to_centroid(copy_DT, coords = coords, crs = utm)[
      is.na(X), is.na(distance_centroid)
    ]
  )

  copy_DT <- copy(DT)[sample(.N, 1e3)]
  copy_DT[sample(.N, 1e2), Y := NA]
  expect_all_true(
    distance_to_centroid(copy_DT, coords = coords, crs = utm)[
      is.na(Y), is.na(distance_centroid)
    ]
  )

  copy_DT <- copy(DT)[sample(.N, 1e3)]
  copy_DT[sample(.N, 1e2), centroid_X := NA]
  expect_all_true(
    distance_to_centroid(copy_DT, coords = coords, crs = utm)[
      is.na(centroid_X), is.na(distance_centroid)
    ]
  )

  copy_DT <- copy(DT)[sample(.N, 1e3)]
  copy_DT[sample(.N, 1e2), centroid_Y := NA]
  expect_all_true(
    distance_to_centroid(copy_DT, coords = coords, crs = utm)[
      is.na(centroid_Y), is.na(distance_centroid)
    ]
  )

  # geometry
  copy_DT <- copy(DT)[sample(.N, 1e3)]
  copy_DT[sample(.N, 1e2), geometry := st_sfc(st_point())]
  expect_all_true(
    distance_to_centroid(copy_DT)[
      sf::st_is_empty(geometry), is.na(distance_centroid)
    ]
  )

  copy_DT <- copy(DT)[sample(.N, 1e3)]
  copy_DT[sample(.N, 1e2), centroid := st_sfc(st_point())]
  expect_all_true(
    distance_to_centroid(copy_DT)[
      sf::st_is_empty(centroid), is.na(distance_centroid)
    ]
  )

  copy_DT <- copy(DT)[sample(.N, 1e3)]
  get_geometry(copy_DT, coords = coords, crs = utm, output_crs = 4326,
               geometry_colname = 'geometry_longlat')
  centroid_group(copy_DT, geometry = 'geometry_longlat')
  copy_DT[sample(.N, 1e2), geometry_longlat := st_sfc(st_point())]
  expect_all_true(
    distance_to_centroid(copy_DT, geometry = 'geometry_longlat')[
      sf::st_is_empty(geometry_longlat), is.na(distance_centroid)
    ]
  )

  copy_DT <- copy(DT)[sample(.N, 1e3)]
  get_geometry(copy_DT, coords = coords, crs = utm, output_crs = 4326,
               geometry_colname = 'geometry_longlat')
  centroid_group(copy_DT, geometry = 'geometry_longlat')
  copy_DT[sample(.N, 1e2), centroid := st_sfc(st_point())]
  expect_all_true(
    distance_to_centroid(copy_DT)[
      sf::st_is_empty(centroid), is.na(distance_centroid)
    ]
  )
})
