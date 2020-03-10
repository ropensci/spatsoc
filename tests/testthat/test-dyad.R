context("test-edge-nn")

library(spatsoc)

DT <- fread('../testdata/DT.csv')
DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
group_times(DT, datetime = 'datetime', threshold = '20 minutes')
edges <- edge_nn(DT,
                 id = 'ID',
                 coords = c('X', 'Y'),
                 timegroup = 'timegroup')


# TODO: Test if any are NULL
# TODO: Test if id1 or id2 arent in DT
