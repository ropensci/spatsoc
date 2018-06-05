# Test Group Lines
context('test GroupLines')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')
utm <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

DT[, family := sample(c(1, 2, 3, 4), .N, replace = TRUE)]
DT[, datetime := as.POSIXct(datetime)]
GroupTimes(DT, timeField = 'datetime', threshold = '14 days')

test_that('one of DT or spLines is required, not both or neither', {
  expect_error(GroupLines(DT = NULL, bufferWidth = 10, spLines = NULL),
               'must provide either DT or spLines')

  expect_error(GroupLines(DT = DT, bufferWidth = 10,
                          spLines = BuildLines(DT, projection = utm,
                                               coordFields = c('X', 'Y'),
                                               idField = 'ID')),
               'cannot provide both DT and spLines')
})


# expect_error(GroupLines(DT = DT, bufferWidth = 10, timeGroup = 'timegroup',
#                         groupFields = 'family', idField = 'ID',
#                         coordFields = c('X', 'Y'), projection = utm,
#                         spLines = NULL)


test_that('coordFields, idField, projection must be provided and proper format', {
  expect_error(GroupLines(DT = DT,
                          bufferWidth = 10, timeGroup = 'timegroup',
                          idField = NULL, coordFields = c('X', 'Y'),
                          projection = utm),
               'idField must be provided')

  expect_error(GroupLines(DT = DT, bufferWidth = 10, timeGroup = 'timegroup',
                          idField = 'ID', coordFields = c('X', 'Y'),
                          projection = NULL),
               'projection must be provided', fixed = FALSE)

  expect_error(GroupLines(DT = DT, bufferWidth = 10, timeGroup = 'timegroup',
                          idField = 'ID', coordFields = NULL,
                          projection = utm),
               'coordFields must be provided')

})


test_that('column names must exist in DT', {
  expect_error(GroupLines(DT = DT, bufferWidth = 10, timeGroup = 'timegroup',
                          idField = 'potatoID', coordFields = c('X', 'Y'),
                          projection = utm),
               'not present in input DT', fixed = FALSE)

  expect_error(GroupLines(DT = DT, bufferWidth = 10, timeGroup = 'timegroup',
                          idField = 'ID', coordFields = c('potatoX', 'potatoY'),
                          projection = utm),
               'not present in input DT', fixed = FALSE)
})


test_that('buffer width is correctly provided, or error', {
  copyDT <- copy(DT)
  expect_warning(GroupLines(DT = copyDT, bufferWidth = NULL, timeGroup = 'timegroup',
                          idField = 'ID', coordFields = c('X', 'Y'),
                          projection = utm),
               'buffer width missing, using 0 by default')

  expect_error(GroupLines(DT = DT, bufferWidth = -10, timeGroup = 'timegroup',
                            idField = 'ID', coordFields = c('X', 'Y'),
                            projection = utm),
                 'cannot provide a negative bufferWidth')
})


test_that('spLines provied must be an S4 + spatial lines', {
  expect_error(GroupLines(spLines = DT, bufferWidth = 10),
               'spLines provided must be a SpatialLines object')
})

# GroupLines(
#   DT = DT,
#   bufferWidth = 10,
#   timeField = 'timegroup',
#   groupFields = 'family',
#   idField = 'ID',
#   coordFields = c('X', 'Y'),
#   projection = utm
# )
