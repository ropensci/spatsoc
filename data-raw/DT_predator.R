## code to prepare `DT_predator` dataset goes here
DT <- fread('inst/extdata/DT.csv')

DT_predator <- DT[ID %in% LETTERS[seq.int(3)]]

DT_predator[, c('X_trans', 'Y_trans') := .(X + 1e4, Y - 5e4)]
DT_predator[, type := 'predator']

# usethis::use_data(DT_predator, overwrite = TRUE)
fwrite(DT_predator, 'inst/extdata/DT_predator.csv')
