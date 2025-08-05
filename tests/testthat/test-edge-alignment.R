# Test edge_alignment
context('test-edge-alignment')

library(spatsoc)

id <- 'ID'
coords <- c('X', 'Y')
direction <- 'direction'
timegroup <- 'timegroup'
group <- 'group'
projection <- 32736

DT <- fread('../testdata/DT.csv')
group_times(DT, 'datetime', '10 minutes')
direction_step(
  DT = DT,
  id = id,
  coords = coords,
  projection = projection
)

test_that('required arguments are provided else error', {
  expect_error(edge_alignment(DT = NULL),
               'DT')

  expect_error(edge_alignment(DT, id = NULL),
               'id')

  expect_error(edge_alignment(DT, id, direction = NULL),
               'direction')

  expect_error(edge_alignment(DT, id, direction, timegroup = NULL),
               'timegroup')
})

test_that('column names must exist in DT', {
  expect_error(edge_alignment(DT, id = 'potato', direction, timegroup),
               'field')

  expect_error(edge_alignment(DT, id, direction = 'potato', timegroup),
               'field')

  expect_error(edge_alignment(DT, id, direction, timegroup = 'potato'),
               'field')

  expect_error(edge_alignment(DT, id, direction, timegroup, group = 'potato'),
               'field')

  expect_error(edge_alignment(DT, id, direction, timegroup, splitBy = 'potato'),
               'field')
})

test_that('direction is units, timegroup is integer, signed is logical', {
  expect_error(edge_alignment(DT, id, direction = 'X', timegroup),
               'units')

  expect_warning(edge_alignment(DT, id, direction, timegroup = 'ID'),
                 'timegroup')

  expect_error(edge_alignment(DT, id, direction, timegroup, signed = 42),
               'signed')
})

test_that('duplicate IDs in a timegroup detected', {
  copyDT <- copy(DT)[, datetime := as.POSIXct(datetime)]
  group_times(copyDT, datetime = 'datetime', threshold = '8 hours')
  expect_warning(edge_alignment(copyDT, id, direction, timegroup),
                 'duplicate')
})

test_that('warns about splitBy column', {
  copyDT <- copy(DT)
  group_times(copyDT, 'datetime', '5 minutes')
  copyDT[, splitBy := as.IDate(datetime)]

  expect_warning(edge_alignment(copyDT, id, direction, timegroup),
                 'split_by')
})

test_that('returned IDs make sense', {
  eDT <- edge_alignment(DT, id, direction, timegroup)

  IDs <- DT[, unique(ID)]
  expect_true(all(eDT$ID1 %in% IDs))
  expect_true(all(eDT$ID2 %in% IDs))
  expect_true(eDT[ID1 == ID2, .N] == 0)
})

test_that('returns a data.table', {
  expect_s3_class(edge_alignment(DT, id, direction, timegroup),
                  'data.table')
})
