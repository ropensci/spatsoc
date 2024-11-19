#' Directional correlation delay based edge lists
#'
#' Temporal delay in absolute bearing between individuals
#'
#' @param DT relocation data
#' @param edges edges generated with edges_dist
#' @param window integer window in timegroups generated with group_times
edge_delay <- function(DT, id = NULL, edges, window = NULL) {
  stopifnot(!is.null(id))
  stopifnot(!is.null(window))

  stopifnot(id %in% colnames(DT))

  setnames(DT, id, 'id')

  stopifnot('dyadID' %in% colnames(edges))
  stopifnot('timegroup' %in% colnames(edges))
  stopifnot('fusionID' %in% colnames(edges))
  stopifnot('dyadID' %in% colnames(edges))

  stopifnot('bearing' %in% colnames(DT))
  stopifnot('timegroup' %in% colnames(DT))

  # TODO: check window isnt in colnames

  setorder(DT, timegroup)

  id_tg <- edges[!is.na(fusionID), .(
    tg = unique(timegroup),
    dyadID = unique(dyadID),
    ID1 = first(ID1),
    ID2 = first(ID2)
  ), by = fusionID]
  id_tg[, min_tg := data.table::fifelse(tg - window < min(tg), min(tg), tg - window),
        by = fusionID]
  id_tg[, max_tg := data.table::fifelse(tg + window < min(tg), min(tg), tg + window),
        by = fusionID]

  id_tg[, delay_tg := {
    focal_bearing <- DT[timegroup == .BY$tg & id == ID1, bearing]
    DT[between(timegroup, min_tg, max_tg) & id == ID2,
       timegroup[which.min(delta_rad(focal_bearing, bearing))]]
  }, by = .(tg,  dyadID)]

  id_tg[, dir_corr_delay := tg - delay_tg]

  data.table::setnames(id_tg,  c('tg'), c('timegroup'))
  data.table::set(id_tg, j = c('min_tg', 'max_tg','delay_tg'), value = NULL)
  data.table::setorder(id_tg, timegroup, ID1, ID2, dir_corr_delay)

  out <- data.table::rbindlist(list(
    id_tg,
    id_tg[, .(timegroup,  dyadID, fusionID,
              ID1 = ID2, ID2 = ID1, dir_corr_delay = - dir_corr_delay)]
  ), use.names = TRUE)

  return(out)
}
