# Test direction_step
context('test direction_step')

library(spatsoc)

DT <- fread('../testdata/DT.csv')

DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
setorder(DT, datetime)

id <- 'ID'
coords <- c('X', 'Y')
projection <- 32736

clean_DT <- copy(DT)
