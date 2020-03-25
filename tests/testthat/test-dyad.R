context("test-dyad")

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
               'input id1 required')

  expect_error(dyad_id(DT = edges, id1 = 'ID1', id2 = NULL),
               'input id2 required')
})

test_that('columns must be in DT', {
  expect_error(dyad_id(DT = edges, id1 = 'potato', id2 = 'ID2'),
               'provided are not present in input DT', fixed = FALSE)

  expect_error(dyad_id(DT = edges, id1 = 'ID1', id2 = 'potato'),
               'provided are not present in input DT', fixed = FALSE)
})


test_that('dyadID column succesfully detected', {
  edges[, dyadID := 1]
  expect_message(
    dyad_id(DT = edges, id1 = 'ID', id2 = 'NN'),
    'dyadID column will be overwritten'
  )
})



test_that('dyadID handles NAs', {
  naedges <- copy(edges)[, NN := NA]
  expect_equal(dyad_id(DT = naedges,
                       id1 = 'ID',
                       id2 = 'NN')[is.na(dyadID), .N],
               naedges[is.na(NN), .N])

  naedges <- copy(edges)[, ID := NA]
  expect_equal(dyad_id(DT = naedges,
                       id1 = 'ID',
                       id2 = 'NN')[is.na(dyadID), .N],
               naedges[is.na(ID), .N])
})


test_that('dyadID handles numeric ids', {
  nDyads <- dyad_id(copy(edges), 'ID', 'NN')[, uniqueN(dyadID)]
  numids <- copy(edges)
  numids[, ID := .GRP, ID]
  numids[, NN := .GRP, NN]

  expect_equal(dyad_id(DT = numids,
                       id1 = 'ID',
                       id2 = 'NN')[, uniqueN(dyadID)],
               nDyads)

})
