# Test get_geometry
context('test get_geometry')

library(sf)

DT <- fread('../testdata/DT.csv')

coords <- c('X', 'Y')
crs <- 32736
