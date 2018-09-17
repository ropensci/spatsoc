# Test GBI
context('test get_gbi')
library(spatsoc)

DT <- fread('../testdata/DT.csv')

# Cast the character column to POSIXct
DT[, datetime := as.POSIXct(datetime)]
DT[, yr := year(datetime)]

utm <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

group_polys(DT, area = FALSE, hrType = 'mcp',
            hrParams = list(percent = 95),
            projection = utm, id = 'ID', coords = c('X', 'Y'),
            splitBy = 'yr')

DT <- unique(DT[, .(ID, group, yr)])

test_that('DT is required', {
  expect_error(get_gbi(
    DT = NULL,
    group = 'group',
    id = 'ID',
    type = 'point'
  ),
  'input DT required')
})


test_that('ID and group column names provided', {
  expect_error(get_gbi(
    DT = DT,
    group = NULL,
    id = 'ID',
    type = 'point'
  ),
  'group field required')

  expect_error(get_gbi(
    DT = DT,
    group = 'group',
    id = NULL,
    type = 'point'
  ),
  'ID field required')

})


# test_that('type provided, and correctly', {
#   expect_error(get_gbi(
#     DT = DT,
#     group = 'group',
#     id = 'ID',
#     type = NULL
#   ),
#   'type required')
#
#   expect_error(get_gbi(
#     DT = DT,
#     group = 'group',
#     id = 'ID',
#     type = 'potato'
#   ),
#   'type must be one of', fixed = FALSE)
#
# })

test_that('columns in DT', {
  expect_error(get_gbi(
    DT = DT,
    group = 'potato',
    id = 'ID',
    type = 'point'
  ),
  'provided are not present in input DT', fixed = FALSE)

  expect_error(get_gbi(
    DT = DT,
    group = 'group',
    id = 'potato',
    type = 'point'
  ),
  'provided are not present in input DT', fixed = FALSE)

})



test_that('matrix returned', {
  expect_type(
    get_gbi(
      DT = DT,
      group = 'group',
      id = 'ID',
      type = 'point'
    ),
    'integer'
  )

  expect_s3_class(
    get_gbi(
      DT = DT,
      group = 'group',
      id = 'ID',
      type = 'point'
    ),
    'matrix'
  )

})
