# Test Group Lines
context('test GroupLines')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')
utm <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

DT[, family := sample(c(1, 2, 3, 4), .N, replace = TRUE)]
DT[, datetime := as.POSIXct(datetime)]
DT[, jul := data.table::yday(datetime)]

test_that('one of DT or spLines is required, not both or neither', {
  expect_error(GroupLines(DT = NULL, bufferWidth = 10, spLines = NULL),
               'must provide either DT or spLines')

  expect_error(GroupLines(DT = DT, bufferWidth = 10,
                          spLines = BuildLines(DT, projection = utm,
                                               coordFields = c('X', 'Y'),
                                               idField = 'ID')),
               'cannot provide both DT and spLines')
})


test_that('coordFields, idField, projection provided and proper format', {
  copyDT <- copy(DT)
  GroupTimes(copyDT, timeField = 'datetime', threshold = '14 days')
  expect_error(GroupLines(DT = copyDT,
                          bufferWidth = 10, timeGroup = 'timegroup',
                          idField = NULL, coordFields = c('X', 'Y'),
                          projection = utm),
               'idField must be provided')

  expect_error(GroupLines(DT = copyDT, bufferWidth = 10, timeGroup = 'timegroup',
                          idField = 'ID', coordFields = c('X', 'Y'),
                          projection = NULL),
               'projection must be provided', fixed = FALSE)

  expect_error(GroupLines(DT = copyDT, bufferWidth = 10, timeGroup = 'timegroup',
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
  GroupTimes(copyDT, timeField = 'datetime', threshold = '14 days')
  expect_warning(GroupLines(DT = copyDT, bufferWidth = NULL, timeGroup = 'timegroup',
                          idField = 'ID', coordFields = c('X', 'Y'),
                          projection = utm),
               'buffer width missing, using 0 by default')

  expect_error(GroupLines(DT = DT, bufferWidth = -10, timeGroup = 'timegroup',
                            idField = 'ID', coordFields = c('X', 'Y'),
                            projection = utm),
                 'cannot provide a negative bufferWidth')
})


test_that('spLines provided must be an S4 + spatial lines', {
  expect_error(GroupLines(spLines = DT, bufferWidth = 10),
               'spLines provided must be a SpatialLines object')
})

test_that('group lines returns a single warning for <2 locs', {
  copyDT <- copy(DT)
  GroupTimes(copyDT, timeField = 'datetime', threshold = '2 days')
  expect_warning(GroupLines(DT = copyDT, bufferWidth = 10, timeGroup = 'timegroup',
                            idField = 'ID', coordFields = c('X', 'Y'),
                            projection = utm),
               'some rows were dropped, cannot build a line with', fixed = FALSE)

  # expect equal sum < 2
  # copyDT <- copy(DT)
  # GroupTimes(copyDT, timeField = 'datetime', threshold = '2 days')
  # expect_equal(length(GroupLines(DT = copyDT, bufferWidth = 10,
  #                                timeGroup = 'timegroup',
  #                                idField = 'ID', coordFields = c('X', 'Y'),
  #                                projection = utm))[!is.na(group)],
  #              sum(copyDT[, .N >= 2, by = timegroup]))
})

test_that('group column is added to result', {
  copyDT <- copy(DT)
  GroupTimes(copyDT, timeField = 'datetime', threshold = '14 days')

  expect_true('group' %in%
                colnames(
                  GroupLines(
                    DT = copyDT,
                    bufferWidth = 10,
                    timeGroup = 'timegroup',
                    idField = 'ID',
                    coordFields = c('X', 'Y'),
                    projection = utm
                  )
                ))
})

test_that('only one column added to the result DT', {
  copyDT <- copy(DT)
  GroupTimes(copyDT, timeField = 'datetime', threshold = '14 days')

  expect_equal(ncol(copyDT) + 1,
               ncol(GroupLines(DT = copyDT, bufferWidth = 10,
                               timeGroup = 'timegroup', idField = 'ID',
                               coordFields = c('X', 'Y'), projection = utm)))
})

test_that('no rows are added to the result DT', {
  copyDT <- copy(DT)
  GroupTimes(copyDT, timeField = 'datetime', threshold = '14 days')

  expect_equal(nrow(copyDT),
               nrow(GroupLines(DT = copyDT, bufferWidth = 10,
                               timeGroup = 'timegroup', idField = 'ID',
                               coordFields = c('X', 'Y'), projection = utm)))
})


test_that('withinGroup is not returned to the user', {
  copyDT <- copy(DT)
  GroupTimes(copyDT, timeField = 'datetime', threshold = '14 days')

  expect_false('withinGroup' %in% colnames(
    GroupLines(DT = copyDT, bufferWidth = 10,
                               timeGroup = 'timegroup', idField = 'ID',
                               coordFields = c('X', 'Y'), projection = utm)))
})

# test that
test_that('only 1 unique timeGroup * groupFields', {
  copyDT <- DT[, mnth := month(datetime)][, yr := year(datetime)]

  GroupLines(
    DT = copyDT,
    bufferWidth = 100,
    timeGroup = 'mnth',
    idField = 'ID',
    coordFields = c('X', 'Y'),
    projection = utm,
    groupFields = 'yr'
  )
  expect_equal(
    copyDT[, .(uniqueMonths = uniqueN(mnth)),
           by = .(group)][, max(uniqueMonths)],
    1)

  expect_equal(
    copyDT[, .(uniqueYears = uniqueN(yr)),
           by = .(group)][, max(uniqueYears)],
    1)

})
# or uniquen(.SD, by = c(idField, groupFields))


# GroupLines(
#   DT = DT,
#   bufferWidth = 10,
#   timeField = 'timegroup',
#   groupFields = 'family',
#   idField = 'ID',
#   coordFields = c('X', 'Y'),
#   projection = utm
# )
