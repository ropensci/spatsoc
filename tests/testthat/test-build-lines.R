# Test build_lines
context('test build_lines')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')
utm <-
  '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'


test_that('DT is required', {
  expect_error(
    build_lines(
      DT = NULL,
      idField = 'ID',
      coordFields = c('X', 'Y'),
      projection = utm
    ),
    'input DT required'
  )
})

test_that('coordFields, idField, projection must be provided and proper format',
          {
            expect_error(
              build_lines(
                DT = DT,
                idField = NULL,
                coordFields = c('X', 'Y'),
                projection = utm
              ),
              'idField must be provided'
            )

            expect_error(
              build_lines(
                DT = DT,
                idField = 'ID',
                coordFields = c('X', 'Y'),
                projection = NULL
              ),
              'projection must be provided'
            )

            expect_error(
              build_lines(
                DT = DT,
                idField = 'ID',
                coordFields = NULL,
                projection = utm
              ),
              'coordFields must be provided'
            )

            expect_error(
              build_lines(
                DT = DT,
                idField = 'ID',
                coordFields = c('ID', 'ID'),
                projection = utm
              ),
              'coordFields must be numeric'
            )
          })


test_that('column names must exist in DT', {
  expect_error(
    build_lines(
      DT = DT,
      idField = 'ID',
      coordFields = c('potatoX', 'potatoY'),
      projection = utm
    ),
    'not present in input DT',
    fixed = FALSE
  )

  expect_error(
    build_lines(
      DT = DT,
      idField = 'potato',
      coordFields = c('X', 'Y'),
      projection = utm
    ),
    'not present in input DT',
    fixed = FALSE
  )
})

test_that('returns same number of lines as unique IDs/groupFields provided', {
  # without groupFields
  expect_equal(length(
    build_lines(
      DT = DT,
      idField = 'ID',
      coordFields = c('X', 'Y'),
      projection = utm
    )
  ),
  DT[, uniqueN(ID)])

  # with groupFields
  DT[, jul := data.table::yday(as.POSIXct(datetime))]
  groupFields = c('ID', 'jul')
  DT[, count := .N, by = groupFields]
  subDT <- DT[count >= 2]

  expect_equal(length(
    build_lines(
      DT = subDT,
      idField = 'ID',
      coordFields = c('X', 'Y'),
      projection = utm,
      groupFields = 'jul'
    )
  ),
  nrow(unique(subDT[, .SD, .SDcols = groupFields])))
})


test_that("build lines warns if < 2 locs per ID/byField", {
  # for ID (one row's ID is "potato")
  copyDT <- copy(DT)[1, ID := 'potato']

  expect_warning(
    build_lines(
      DT = copyDT,
      idField = 'ID',
      coordFields = c('X', 'Y'),
      projection = utm
    ),
    'some rows dropped, cannot build lines with less than two points'
  )


  # for ID + groupFields
  groupFields = c('ID', 'jul')
  DT[, jul := data.table::yday(as.POSIXct(datetime))]
  DT[, count := .N, by = groupFields]
  subDT <- DT[count < 2]

  expect_warning(
    build_lines(
      DT = subDT,
      idField = 'ID',
      coordFields = c('X', 'Y'),
      projection = utm,
      groupFields = 'jul'
    ),
    'some rows dropped, cannot build',
    fixed = FALSE
  )
})

test_that('groupFields and idField provided are not correct format', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_error(
    build_lines(
      DT = copyDT,
      idField = 'datetime',
      coordFields = c('X', 'Y'),
      projection = utm
    ),
    'idField \\(and groupFields when provided\\) must',
    fixed = FALSE
  )

  expect_error(
    build_lines(
      DT = copyDT,
      idField = 'ID',
      coordFields = c('X', 'Y'),
      projection = utm,
      groupFields = 'datetime'
    ),
    'idField \\(and groupFields when provided\\) must be',
    fixed = FALSE
  )

  # with factor IDs
  copyDT <- copy(DT)[, ID := as.factor(ID)]
  expect_error(
    build_lines(
      DT = copyDT,
      idField = 'ID',
      coordFields = c('X', 'Y'),
      projection = utm
    ),
    'idField \\(and groupFields when provided\\) must be',
    fixed = FALSE
  )
})

test_that('BuildPts returns a SpatialLines', {
  expect_true('SpatialLines' %in% class(
    build_lines(
      DT = DT,
      idField = 'ID',
      coordFields = c('X', 'Y'),
      projection = utm
    )
  ))

  expect_true(isS4(
    build_lines(
      DT = DT,
      idField = 'ID',
      coordFields = c('X', 'Y'),
      projection = utm
    )
  ))
})


# build_lines(
#   DT = DT,
#   idField = 'ID',
#   coordFields = c('X', 'Y'),
#   projection = utm
# )
