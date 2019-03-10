group_nn <- function(DT, threshold, id, coords, timegroup, splitBy) {
  DT[, nn := {
    nn(.SD)
    nn[nn < threshold]
  },
  c(timegroup, splitBy)]
}

DT
group_nn(DT)
# ??
DT <- DT[!is.na(nn)]
graph_from_data_frame(DT[, .(id, nn, timegroup)])
