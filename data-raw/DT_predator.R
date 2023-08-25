library(data.table)
DT <- fread('inst/extdata/DT.csv')

DT_predator <- DT[ID %in% LETTERS[seq.int(2)]][sample(.N, 100)]

DT_predator[, c('X', 'Y') := .(X + 1e4, Y - 5e4)]
DT_predator[, type := 'predator']

# usethis::use_data(DT_predator, overwrite = TRUE)
fwrite(DT_predator, 'inst/extdata/DT_predator.csv')
