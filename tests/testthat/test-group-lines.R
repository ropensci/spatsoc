# Test Group Lines
context('test GroupLines')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')
utm <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

DT[, family := sample(c(1, 2, 3, 4), .N, replace = TRUE)]
DT[, datetime := as.POSIXct(datetime)]
GroupTimes(DT, timeField = 'datetime', threshold = '14 days')

test_that('one of DT or spLines is required, not both or neither', {
  expect_error(GroupLines(DT = NULL, spLines = NULL),
               'must provide either DT or spLines')

  expect_error(GroupLines(DT = DT,
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
  expect_warning(GroupLines(DT = DT, bufferWidth = NULL, timeGroup = 'timegroup',
                          idField = 'ID', coordFields = c('X', 'Y'),
                          projection = utm),
               'buffer width missing, using 0 by default')

  expect_stop(GroupLines(DT = DT, bufferWidth = -10, timeGroup = 'timegroup',
                            idField = 'ID', coordFields = c('X', 'Y'),
                            projection = utm),
                 'cannot provide a negative bufferWidth')

})


# test_that('returns same number of lines as unique IDs/byFields provided', {
#   # without byFields
#   expect_equal(length(
#     BuildLines(
#       DT = DT,
#       idField = 'ID',
#       coordFields = c('X', 'Y'),
#       projection = utm
#     )
#   ),
#   DT[, uniqueN(ID)])
#
#   # with byFields
#   DT[, jul := data.table::yday(as.POSIXct(datetime))]
#   byFields = c('ID', 'jul')
#   DT[, count := .N, by = byFields]
#   subDT <- DT[count >= 2]
#
#   expect_equal(length(
#     BuildLines(
#       DT = subDT,
#       idField = 'ID',
#       coordFields = c('X', 'Y'),
#       projection = utm,
#       byFields = 'jul'
#     )
#   ),
#   nrow(unique(subDT[, .SD, .SDcols = byFields])))
# })
#
#
# test_that("build lines warns if < 2 locs per ID/byField", {
#   # for ID (one row's ID is "potato")
#   copyDT <- copy(DT)[1, ID := 'potato']
#
#   expect_warning(BuildLines(DT = copyDT, idField = 'ID',
#                             coordFields = c('X', 'Y'),
#                             projection = utm),
#                  'some rows dropped, cannot build lines with less than two points')
#
#
#   # for ID + byFields
#   byFields = c('ID', 'jul')
#   DT[, jul := data.table::yday(as.POSIXct(datetime))]
#   DT[, count := .N, by = byFields]
#   subDT <- DT[count < 2]
#
#   expect_warning(BuildLines(DT = subDT, idField = 'ID',
#                             coordFields = c('X', 'Y'),
#                             projection = utm, byFields = 'jul'),
#                  'some rows dropped, cannot build', fixed = FALSE)
# })
#
# test_that('byFields and idField provided are not correct format', {
#   copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
#   expect_error(BuildLines(DT = copyDT, idField = 'datetime',
#                           coordFields = c('X', 'Y'),
#                           projection = utm),
#                'idField \\(and byFields when provided\\) must', fixed = FALSE)
#
#   expect_error(BuildLines(DT = copyDT, idField = 'ID',
#                           coordFields = c('X', 'Y'),
#                           projection = utm, byFields = 'datetime'),
#                'idField \\(and byFields when provided\\) must be', fixed = FALSE)
#
#   # with factor IDs
#   copyDT <- copy(DT)[, ID := as.factor(ID)]
#   expect_error(BuildLines(DT = copyDT, idField = 'ID',
#                           coordFields = c('X', 'Y'),
#                           projection = utm),
#                'idField \\(and byFields when provided\\) must be', fixed = FALSE)
# })

# GroupLines(
#   DT = DT,
#   bufferWidth = 10,
#   timeField = 'timegroup',
#   groupFields = 'family',
#   idField = 'ID',
#   coordFields = c('X', 'Y'),
#   projection = utm
# )
