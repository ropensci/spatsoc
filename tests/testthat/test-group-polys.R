# Test group_polys
context('test group_polys')
library(spatsoc)

DT <- fread('../testdata/DT.csv')

utm <-
  '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

test_that('DT or spPts are required but not both', {
  expect_error(group_polys(
    DT = NULL,
    spPolys = NULL,
    area = FALSE
  ),
  'must provide either DT or spPolys')

  expect_error(group_polys(
    DT = DT,
    spPolys = 10,
    area = FALSE
  ),
  'cannot provide both DT and spPolys')
})

test_that('area provided and logical, or error', {
  expect_error(
    group_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      area = NULL,
      coords = c('X', 'Y'),
      id = 'ID'
    ),
    'area must be provided',
    fixed = TRUE
  )

  expect_error(
    group_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      area = 'potato',
      coords = c('X', 'Y'),
      id = 'ID'
    ),
    'area must be provided',
    fixed = TRUE
  )

  expect_error(
    group_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      area = 10,
      coords = c('X', 'Y'),
      id = 'ID'
    ),
    'area must be provided',
    fixed = TRUE
  )
})


test_that('projection provided or error', {
  expect_error(
    group_polys(
      DT = DT,
      projection = NULL,
      hrType = 'mcp',
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID'
    ),
    'projection must be provided'
  )
})

test_that('mising hrParams warns default used', {
  copyDT <- copy(DT)
  expect_message(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID'
    ),
    'hrParams is not provided, using defaults'
  )
})

test_that('missing hrType fails', {
  expect_error(
    group_polys(
      DT = DT,
      projection = utm,
      hrType = NULL,
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID'
    ),
    'hrType must be provided'
  )
})

test_that('column names must exist in DT', {
  expect_error(
    group_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'potato'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  expect_error(
    group_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      area = FALSE,
      coords = c('potatoX', 'potatoY'),
      id = 'ID'
    ),
    'not present in input DT',
    fixed = FALSE
  )

  expect_error(
    group_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'potato'
    ),
    'not present in input DT',
    fixed = FALSE
  )
})

test_that('splitBy and spPolys are not both provided', {
  expect_error(
    group_polys(
      splitBy = 'yr',
      spPolys = 10,
      area = TRUE
    ),
    'cannot provide spPolys if providing splitBy'
  )
})


test_that('ID field is alphanumeric and does not have spaces', {
  copyDT <- copy(DT)[ID == unique(ID)[1], ID := paste(ID, ID)]
  expect_error(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coords = c('X', 'Y'),
      id = 'ID'
    ),
    'please ensure IDs are alphanumeric and do not contain spaces'
  )

  copyDT[, population := 1]
  expect_error(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'population'
    ),
    'please ensure IDs are alphanumeric and do not contain spaces'
  )
})

test_that('column and row lengths returned make sense', {
  expect_lte(nrow(
    group_polys(
      DT = DT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coords = c('X', 'Y'),
      id = 'ID'
    )
  ),
  nrow(expand.grid(DT[, unique(ID)], DT[, unique(ID)])))

  copyDT <- copy(DT)[, yr := year(datetime)]
  expect_equal(nrow(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'yr'
    )
  ),
  nrow(copyDT))

  copyDT <- copy(DT)
  copyDT[, family := sample(c(1, 2, 3, 4), .N, replace = TRUE)]
  expect_equal(ncol(copyDT) + 1,
               ncol(
                 group_polys(
                   DT = copyDT,
                   projection = utm,
                   hrType = 'mcp',
                   hrParams = list(percent = 95),
                   area = FALSE,
                   coords = c('X', 'Y'),
                   id = 'ID',
                   splitBy = 'family'
                 )
               ))
})


test_that('withinGroup is not returned to the user', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  # to avoid block length warning
  suppressWarnings(
    group_times(copyDT, datetime = 'datetime', threshold = '14 days')
  )
  copyDT[, N := .N, by = .(ID, block)]
  expect_false('withinGroup' %in% colnames(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID'
    )
  ))
})


test_that('group column succesfully detected', {
  copyDT <- copy(DT)[, group := 1][, mnth := month(datetime)]
  copyDT <- copyDT[, nBy := .N, by = .(mnth, ID)][nBy > 30]
  expect_message(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'mnth'
    ),
    'group column will be overwritten'
  )

  copyDT <- copy(DT)[, group := 1][, mnth := month(datetime)]
  copyDT <- copyDT[, nBy := .N, by = .(mnth, ID)][nBy > 30]
  expect_message(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID'
    ),
    'group column will be overwritten'
  )
})


test_that('area provided with splitBy does not return errors', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  copyDT[, yr := year(datetime)]
  expect_false('withinGroup' %in% colnames(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'yr'
    )
  ))

  expect_true('area' %in% colnames(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'yr'
    )
  ))

  expect_true('proportion' %in% colnames(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'yr'
    )
  ))
})


test_that('less than 5 locs returns NAs and warning', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  copyDT[, yr := year(datetime)]
  copyDT[1, yr := 2020]
  expect_warning(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'yr'
    ),
    'build_polys failed for some rows', fixed = FALSE
  )

  expect_warning(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'yr'
    ),
    'build_polys failed for some rows', fixed = FALSE
  )

  expect_true(suppressWarnings(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = FALSE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'yr'
    )[is.na(group), .N] != 0)
  )

  expect_true(suppressWarnings(
    group_polys(
      DT = copyDT,
      projection = utm,
      hrType = 'mcp',
      hrParams = list(percent = 95),
      area = TRUE,
      coords = c('X', 'Y'),
      id = 'ID',
      splitBy = 'yr'
    )[is.na(area), .N] != 0)
  )


})

# group_polys(
#   DT = DT,
#   projection = utm,
#   hrType = 'mcp',
#   hrParams = NULL,
#   area = FALSE,
#   coords = c('X', 'Y'),
#   id = 'ID',
#   splitBy = NULL,
#   spPolys = NULL
# )
