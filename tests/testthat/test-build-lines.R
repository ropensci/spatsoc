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
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm
    ),
    'input DT required'
  )
})

test_that('coords, id, projection must be provided and proper format',
          {
            expect_error(
              build_lines(
                DT = DT,
                id = NULL,
                coords = c('X', 'Y'),
                projection = utm
              ),
              'id must be provided'
            )

            expect_error(
              build_lines(
                DT = DT,
                id = 'ID',
                coords = c('X', 'Y'),
                projection = NULL
              ),
              'projection must be provided'
            )

            expect_error(
              build_lines(
                DT = DT,
                id = 'ID',
                coords = NULL,
                projection = utm
              ),
              'coords must be provided'
            )

            expect_error(
              build_lines(
                DT = DT,
                id = 'ID',
                coords = c('ID', 'ID'),
                projection = utm
              ),
              'coords must be numeric'
            )
            expect_error(
              build_lines(
                DT = DT,
                id = 'ID',
                coords = c('X'),
                projection = utm
              ),
              'coords requires a vector', fixed = FALSE
            )
          })


test_that('column names must exist in DT', {
  expect_error(
    build_lines(
      DT = DT,
      id = 'ID',
      coords = c('potatoX', 'potatoY'),
      projection = utm
    ),
    'not present in input DT',
    fixed = FALSE
  )

  expect_error(
    build_lines(
      DT = DT,
      id = 'potato',
      coords = c('X', 'Y'),
      projection = utm
    ),
    'not present in input DT',
    fixed = FALSE
  )
})

test_that('returns same number of lines as unique IDs/splitBy provided', {
  # without splitBy
  expect_equal(length(
    build_lines(
      DT = DT,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm
    )
  ),
  DT[, uniqueN(ID)])

  # with splitBy
  DT[, jul := data.table::yday(as.POSIXct(datetime))]
  splitBy <- c('ID', 'jul')
  DT[, count := .N, by = splitBy]
  subDT <- DT[count >= 2]

  expect_equal(length(
    build_lines(
      DT = subDT,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      splitBy = 'jul'
    )
  ),
  nrow(unique(subDT[, .SD, .SDcols = splitBy])))
})


test_that("build lines warns if < 2 locs per ID/byField", {
  # for ID (one row's ID is "potato")
  copyDT <- copy(DT)[1, ID := 'potato']

  expect_warning(
    build_lines(
      DT = copyDT,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm
    ),
    'some rows dropped, cannot build lines with less than two points'
  )


  # for ID + splitBy
  splitBy <- c('ID', 'jul')
  DT[, jul := data.table::yday(as.POSIXct(datetime))]
  DT[, count := .N, by = splitBy]
  subDT <- DT[count < 2]

  expect_warning(
    build_lines(
      DT = subDT,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      splitBy = 'jul'
    ),
    'some rows dropped, cannot build',
    fixed = FALSE
  )
})

test_that('splitBy and id provided are not correct format', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_error(
    build_lines(
      DT = copyDT,
      id = 'datetime',
      coords = c('X', 'Y'),
      projection = utm
    ),
    'id \\(and splitBy when provided\\) must',
    fixed = FALSE
  )

  expect_error(
    build_lines(
      DT = copyDT,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      splitBy = 'datetime'
    ),
    'id \\(and splitBy when provided\\) must be',
    fixed = FALSE
  )

  # with factor IDs
  copyDT <- copy(DT)[, ID := as.factor(ID)]
  expect_error(
    build_lines(
      DT = copyDT,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm
    ),
    'id \\(and splitBy when provided\\) must be',
    fixed = FALSE
  )
})

test_that('BuildPts returns a SpatialLines', {
  expect_true('SpatialLines' %in% class(
    build_lines(
      DT = DT,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm
    )
  ))

  expect_true(isS4(
    build_lines(
      DT = DT,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm
    )
  ))
})


# build_lines(
#   DT = DT,
#   id = 'ID',
#   coords = c('X', 'Y'),
#   projection = utm
# )
