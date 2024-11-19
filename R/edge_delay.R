#' Directional correlation delay based edge lists
#'
#' \code{edge_delay} returns edge lists defined by the directional correlation
#' delay between individuals. The function expects a \code{data.table} with
#' relocation data, distance based edge lists, individual identifiers and a window argument. The
#' window argument is used to specify the temporal window within which to consider
#' the directional correlation delay. Relocation data should be in two columns
#' representing the X and Y coordinates.
#'
#' The \code{DT} and \code{edges} must be \code{data.table}s. If your data is a
#' \code{data.frame}, you can convert it by reference using
#' \code{\link[data.table:setDT]{data.table::setDT}}.
#'
#' The \code{DT} and \code{edges} are internally matched in this function using
#' the columns \code{timegroup} (from \code{group_times}) and \code{ID1} and
#' \code{ID2} (in \code{edges}, from \code{dyad_id}) with \code{id} (in
#' \code{DT}). This function expects a \code{fusionID} present, generated with
#' the \code{fusion_id} function. The \code{timegroup} argument expects the
#' names of a column in \code{edges} which correspond to the timegroup column.
#' The \code{id}, \code{direction} and \code{timegroup} arguments expect the names
#' of a column in \code{DT} which correspond to the id, direction and
#' timegroup columns.
#'
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
