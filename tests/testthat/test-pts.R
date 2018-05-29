# Test GroupPts
context('test GroupPts')
library(spatsoc)

DT <- fread('..testdata/Buffalo')

ls.params <- list(DT = DT,
                  coordFields = c('X', 'Y'),
                  idField = 'ID',
                  time = 'posix')
