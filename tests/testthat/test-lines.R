# Test Lines
context('test Lines')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')
utm <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'


test_that('DT is required', {
  expect_error(BuildLines(DT = NULL, idField = 'ID',
                          coordFields = c('X', 'Y'),
                          projection = utm),
               'input DT required')
})

test_that('coordFields, idField, projection must be provided and proper format', {
  expect_error(BuildLines(DT = DT, idField = NULL,
                          coordFields = c('X', 'Y'),
                          projection = utm),
               'idField must be provided')

  expect_error(BuildLines(DT = DT, idField = 'ID',
                          coordFields = c('X', 'Y'),
                          projection = NULL),
               'projection must be provided')

  expect_error(BuildLines(DT = DT, idField = 'ID',
                          coordFields = NULL,
                          projection = utm),
               'coordFields must be provided')

  expect_error(BuildLines(DT = DT, idField = 'ID',
                          coordFields = c('ID', 'ID'),
                          projection = utm),
               'coordFields must be numeric')
})


test_that('column names must exist in DT', {
  expect_error(BuildLines(DT = DT, idField = 'ID',
                          coordFields = c('potatoX', 'potatoY'),
                          projection = utm),
               'not present in input DT', fixed = FALSE)

  expect_error(BuildLines(DT = DT, idField = 'potato',
                          coordFields = c('X', 'Y'),
                          projection = utm),
               'not present in input DT', fixed = FALSE)
})

test_that('returns same number of lines as unique IDs/byFields provided', {
  # without byFields
  expect_equal(length(
    BuildLines(
      DT = DT,
      idField = 'ID',
      coordFields = c('X', 'Y'),
      projection = utm
    )
  ),
  DT[, uniqueN(ID)])

  # with byFields
  DT[, jul := data.table::yday(as.POSIXct(datetime))]
  byFields = c('ID', 'jul')
  DT[, count := .N, by = byFields]
  subDT <- DT[count >= 2]

  expect_equal(length(
    BuildLines(
      DT = subDT,
      idField = 'ID',
      coordFields = c('X', 'Y'),
      projection = utm,
      byFields = 'jul'
    )
  ),
  nrow(unique(subDT[, .SD, .SDcols = byFields])))
})


test_that("build lines warns if < 2 locs per ID/byField", {
  # for ID (one row's ID is "potato")
  copyDT <- copy(DT)[1, ID := 'potato']

  expect_warning(BuildLines(DT = copyDT, idField = 'ID',
                            coordFields = c('X', 'Y'),
                            projection = utm),
                 'some rows dropped, cannot build lines with less than two points')


  # for ID + byFields
  byFields = c('ID', 'jul')
  DT[, jul := data.table::yday(as.POSIXct(datetime))]
  DT[, count := .N, by = byFields]
  subDT <- DT[count < 2]

  expect_warning(BuildLines(DT = subDT, idField = 'ID',
                            coordFields = c('X', 'Y'),
                            projection = utm, byFields = 'jul'),
                 'some rows dropped, cannot build', fixed = FALSE)
})

test_that('byFields and idField provided are not correct format', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  expect_error(BuildLines(DT = copyDT, idField = 'datetime',
                          coordFields = c('X', 'Y'),
                          projection = utm),
               'idField \\(and byFields when provided\\) must', fixed = FALSE)

  expect_error(BuildLines(DT = copyDT, idField = 'ID',
                          coordFields = c('X', 'Y'),
                          projection = utm, byFields = 'datetime'),
               'idField \\(and byFields when provided\\) must be', fixed = FALSE)

  # with factor IDs
  copyDT <- copy(DT)[, ID := as.factor(ID)]
  expect_error(BuildLines(DT = copyDT, idField = 'ID',
                          coordFields = c('X', 'Y'),
                          projection = utm),
               'idField \\(and byFields when provided\\) must be', fixed = FALSE)
})

# if group provided, it isn't a datetime format





# BuildLines(
#   DT = DT,
#   idField = 'ID',
#   coordFields = c('X', 'Y'),
#   projection = utm
# )
