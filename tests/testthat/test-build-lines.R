# Test build_lines
context('test build_lines')

DT <- fread('../testdata/DT.csv')

utm <- 'EPSG:32736'

DT[, datetime := as.POSIXct(datetime)]

test_that('DT is required', {
  expect_error(
    build_lines(
      DT = NULL,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    ),
    'input DT required'
  )
})

test_that('coords, id, projection, sortBy must be provided, proper format', {
  expect_error(
    build_lines(
      DT = DT,
      id = NULL,
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    ),
    'id must be provided'
  )

  expect_error(
    build_lines(
      DT = DT,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = NULL,
      sortBy = 'datetime'
    ),
    'projection must be provided'
  )

  expect_error(
    build_lines(
      DT = DT,
      id = 'ID',
      coords = NULL,
      projection = utm,
      sortBy = 'datetime'
    ),
    'coords must be provided'
  )

  expect_error(
    build_lines(
      DT = DT,
      id = 'ID',
      coords = c('ID', 'ID'),
      projection = utm,
      sortBy = 'datetime'
    ),
    'coords must be numeric'
  )
  expect_error(
    build_lines(
      DT = DT,
      id = 'ID',
      coords = c('X'),
      projection = utm,
      sortBy = 'datetime'
    ),
    'coords requires a vector', fixed = FALSE
  )

  expect_error(
    build_lines(
      DT = DT,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = NULL
    ),
    'sortBy must be provided'
  )
  expect_error(
    build_lines(
      DT = DT,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'ID'
    ),
    'sortBy provided must be', fixed = FALSE
  )

})


test_that('column names must exist in DT', {
  expect_error(
    build_lines(
      DT = DT,
      id = 'ID',
      coords = c('potatoX', 'potatoY'),
      projection = utm,
      sortBy = 'datetime'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  expect_error(
    build_lines(
      DT = DT,
      id = 'potato',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  expect_error(
    build_lines(
      DT = DT,
      id = 'potato',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'potato'
    ),
    'not present in input DT',
    fixed = FALSE
  )
})

test_that('returns same number of lines as unique IDs/splitBy provided', {
  # without splitBy
  expect_equal(nrow(
    build_lines(
      DT = DT,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    )
  ),
  DT[, uniqueN(ID)])

  # with splitBy
  DT[, jul := data.table::yday(as.POSIXct(datetime))]
  splitBy <- c('ID', 'jul')
  DT[, count := .N, by = splitBy]
  subDT <- DT[count >= 2]

  expect_equal(nrow(
    build_lines(
      DT = subDT,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      splitBy = 'jul',
      sortBy = 'datetime'
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
      projection = utm,
      sortBy = 'datetime'
    ),
    'some rows dropped, cannot build lines with less than two points'
  )

  copyDT <- copy(DT)[1, ID := 'potato']
  expect_warning(
    build_lines(
      DT = copyDT,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      splitBy = 'population',
      sortBy = 'datetime'
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
      projection = utm,
      sortBy = 'datetime'
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
      splitBy = 'datetime',
      sortBy = 'datetime'
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
      projection = utm,
      sortBy = 'datetime'
    ),
    'id \\(and splitBy when provided\\) must be',
    fixed = FALSE
  )
})

test_that('build_lines returns an sf object with LINESTRINGs', {
  expect_s3_class(
    build_lines(
      DT = DT,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    ),
    'sf'
  )

  expect_in(
    'LINESTRING',
    sf::st_geometry_type(build_lines(
      DT = DT,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      sortBy = 'datetime'
    ), by_geometry = FALSE)
  )
})

test_that('build_lines builds ordered lines', {
  base_lines <- build_lines(
    DT = DT,
    id = 'ID',
    coords = c('X', 'Y'),
    projection = utm,
    sortBy = 'datetime'
  )

  random_lines <- build_lines(
    DT = DT[sample(.N)],
    id = 'ID',
    coords = c('X', 'Y'),
    projection = utm,
    sortBy = 'datetime'
  )

  expect_equal(
    base_lines[order(base_lines$ID),],
    random_lines[order(random_lines$ID),]
  )
})




test_that('splitBy argument doesnt use splitBy column', {
  copyDT <- copy(DT)

  copyDT[, splitBy := as.IDate(datetime)]

  utm <- 'EPSG:32736'

  expect_equal(
    nrow(build_lines(
      DT = copyDT,
      id = 'ID',
      coords = c('X', 'Y'),
      projection = utm,
      splitBy = 'splitBy',
      sortBy = 'datetime'
    )),
    copyDT[, uniqueN(splitBy) * uniqueN(ID)]
  )
})
