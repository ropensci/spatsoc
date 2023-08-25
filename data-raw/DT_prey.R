## code to prepare `DT_prey` dataset goes here
DT <- fread('inst/extdata/DT.csv')

DT_prey <- DT[ID %in% LETTERS[seq(4, 7)]]

DT_prey[, c('X_trans', 'Y_trans') := .(X + 1e4, Y - 5e4)]
DT_prey[, type := 'prey']

# usethis::use_data(DT_prey, overwrite = TRUE)
fwrite(DT_prey, 'inst/extdata/DT_prey.csv')
