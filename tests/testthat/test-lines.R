# Test Lines
context('test Lines')
library(spatsoc)

DT <- fread('../testdata/buffalo.csv')
utm <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'


test_that('DT is required', {
  expect_error(GroupLines(DT = NULL, bufferWidth = 10, idField = 'ID',
                          projection = utm),
               'input DT required')
})


