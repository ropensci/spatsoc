context("test-edge-nn")

library(spatsoc)

DT <- fread('../testdata/DT.csv')
DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = 'datetime', threshold = '20 minutes')
edges <- edge_nn(DT,
                 id = 'ID',
                 coords = c('X', 'Y'),
                 timegroup = 'timegroup')


test_that('DT is required', {
  expect_error(dyad_id(DT = NULL),
  'input DT required')
})

test_that('id1 and id2 are required', {
  expect_error(dyad_id(DT = edges, id1 = NULL),
               'input DT required')

  expect_error(dyad_id(DT = edges, id1 = 'ID1', id2 = NULL),
               'input DT required')
})


# TODO: Test if any are NULL
# TODO: Test if id1 or id2 arent in DT
