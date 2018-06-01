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

# BuildLines(
#   DT = DT,
#   idField = 'ID',
#   coordFields = c('X', 'Y'),
#   projection = utm
# )
