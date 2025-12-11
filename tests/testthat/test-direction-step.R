# Test direction_step
context('test direction_step')

library(spatsoc)
library(units)

DT <- fread('../testdata/DT.csv')

DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
setorder(DT, datetime)

id <- 'ID'
coords <- c('X', 'Y')
utm <- 32736

clean_DT <- copy(DT)

test_that('DT is required', {
  expect_error(direction_step(DT = NULL), 'DT must be provided')
})

test_that('args required else error', {
  expect_error(direction_step(DT,  id = NULL), 'id must be')

  expect_error(direction_step(DT,  id = id, coords = coords, crs = NULL),
               'crs must')

  expect_error(direction_step(DT, id = id, coords = 'X', crs = utm),
               'coords must be length 2')
})

test_that('column names must exist in DT', {
  expect_error(direction_step(DT, id = 'potato', coords = coords,
                              crs = utm),
               'not present')

  expect_error(direction_step(DT, id = id, coords = c('potato', 'potato'),
                              crs = utm),
               'not present')

  expect_error(direction_step(DT, id = id, coords = coords,
                              crs = utm, splitBy = 'potato'),
               'not present')
})

test_that('coords are correctly provided or error detected', {
  expect_error(direction_step(DT, id = id, coords = c('X', NULL),
                              crs = utm),
               'coords must be')

  expect_error(direction_step(DT, id = id, coords = c('X', 'ID'),
                              crs = utm),
               'numeric')
})

test_that('dimensions returned expected', {

  expect_equal(
    ncol(clean_DT) + 1,
    ncol(direction_step(copy(clean_DT), id = id,
                        coords = coords, crs = utm))
  )

  expect_equal(
    nrow(clean_DT),
    nrow(direction_step(copy(clean_DT), id = id,
                        coords = coords, crs = utm))
  )

  expect_true('direction' %in% colnames(
    direction_step(
      copy(clean_DT),
      id = id,
      coords = coords,
      crs = utm
    )
  ))

})


test_that('direction column succesfully detected', {
  copyDT <- copy(clean_DT)[, direction := 1]
  expect_message(
    direction_step(
      copyDT,
      id = id,
      coords = coords,
      crs = utm
    ),
    'direction column will be overwritten'
  )
})


test_that('returns a data.table', {
  expect_s3_class(direction_step(DT, id = id, coords = coords,
                                 crs = utm),
                  'data.table')
})


test_that('splitBy returns expected', {
  DT[, split := sample(seq.int(5), .N, replace = TRUE)]
  expect_gte(
    direction_step(DT, id = id, coords = coords,
                   crs = utm,
                   splitBy = 'split')[is.na(direction), .N],
    direction_step(DT, id = id, coords = coords,
                   crs = utm)[is.na(direction), .N]

  )
})


DT_A <- data.table(
  X = c(-5, -5, 0, 14, 10, 0),
  Y = c(5, 3, 1, 1, 11, 11),
  ID =  'A'
)[, timegroup := seq.int(.N)]

# Related to: PR 92
test_that('longlat NA radian returned', {
  expect_equal(
    class(direction_step(DT_A, id = id, coords = coords,
                         crs = 4326)[]$direction),
    'units'
  )
  expect_gte(
    sum(is.na(direction_step(DT_A, id = id, coords = coords,
                             crs = 4326)$direction)),
    1
  )
})


test_that('use_transform errors if crs not provided', {
  expect_error(
    direction_step(DT, id = id, coords = coords, crs = NA),
    'ensure crs is provided'
  )

  copyDT <- copy(DT)
  get_geometry(copyDT, coords = coords, crs = utm)
  st_crs(copyDT$geometry) <- NA
  expect_error(
    direction_step(
      DT = copyDT,
      id = id
    ),
    'ensure crs is provided'
  )
})


DT_B <- data.table(
  X = c(0, 5, 5, 0, 0),
  Y = c(0, 0, 5, 5, 0),
  step = c('E', 'N', 'W', 'S', NA),
  timegroup = seq.int(5),
  ID = 'B'
)
direction_step(DT_B, id, coords, crs = 4326)

test_that('East North West South steps', {
  tolerance <- 0.01

  expect_equal(
    DT_B[step == 'E', direction],
    as_units(pi / 2, 'rad'),
    tolerance = tolerance
  )

  expect_equal(
    DT_B[step == 'N', direction],
    as_units(0),
    tolerance = tolerance
  )

  expect_equal(
    DT_B[step == 'W', direction],
    as_units(- pi / 2),
    tolerance = tolerance
  )

  expect_equal(
    DT_B[step == 'S', direction],
    as_units(pi),
    tolerance = tolerance
  )
})


test_that('projection arg is deprecated', {
  expect_warning(
    direction_step(
      DT,
      id = id,
      coords = coords,
      projection = utm
    ),
    'projection argument is deprecated'
  )
})

# sfc interface

test_that('if coords null, geometry required', {
  expect_error(direction_step(DT, id = id, coords = NULL, crs = utm),
               'get_geometry?')
})

test_that('crs provided with geometry gives message crs ignored', {
  get_geometry(DT, coords = coords, crs = utm)
  expect_message(direction_step(DT, id = id, crs = utm), 'ignored')
})

test_that('geometry correctly provided else error', {
  expect_error(direction_step(DT, id = id, geometry = 'potato'),
               'is not present')
  expect_error(direction_step(DT, id = id, geometry = 'X'),
               'must be of class')
})

test_that('sfc interface message before overwrite', {
  copyDT <- copy(DT)[, direction := 42]
  expect_message(direction_step(copyDT, id = id),
                 'overwritten')
})

test_that('sfc interface returns expected', {
  copyDT <- get_geometry(copy(clean_DT), coords = coords, crs = utm)

  expect_equal(
    ncol(copyDT) + 1,
    ncol(direction_step(copyDT, id = id))
  )

  expect_equal(
    nrow(copyDT),
    nrow(direction_step(copy(copyDT), id = id))
  )

  expect_true('direction' %in% colnames(direction_step(copy(copyDT), id = id)))

  expect_type(copyDT$direction, 'double')
  expect_s3_class(copyDT$direction, 'units')

  expect_equal(min(copyDT$direction, na.rm = TRUE), units::as_units(-pi, 'rad'),
               tolerance = 0.01)
  expect_equal(max(copyDT$direction, na.rm = TRUE), units::as_units(pi, 'rad'),
               tolerance = 0.01)
})

