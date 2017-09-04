# Test Points
# library(spatsoc)
context('test build and group points')

data(locs)

ls.params <- list(DT = locs,
                  projection = '+proj=utm +zone=21 ellps=WGS84',
                  coordFields = c('EASTING', 'NORTHING'),
                  idField = 'ID')

test_that('BuildPts returns SPDF', {
  expect_s4_class(do.call(BuildPts, ls.params), 'SpatialPointsDataFrame')
})

test_that('BuildPts checks colnames exist in DT', {
  expect_error(do.call(BuildPts, c(ls.params[-1])))
  expect_error(do.call(BuildPts, c(ls.params[-2])))
  # these don't throw errors since they have same fields as default
  # expect_error(do.call(BuildPts, c(ls.params[-3])))
  # expect_error(do.call(BuildPts, c(ls.params[-4])))

  # careful that c() and list() dont do the same thing
  expect_error(do.call(BuildPts, list(ls.params[-4], idField = 'turnip')))
  expect_error(do.call(BuildPts, list(ls.params[-3], coordFields = c('potato', 'beet'))))
})

expect_error(do.call(BuildPts, c(ls.params, tuna = 'fish')))
