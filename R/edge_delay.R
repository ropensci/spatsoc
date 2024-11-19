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
#' the \code{fusion_id} function.
#' The \code{id}, and \code{direction} arguments expect the names
#' of a column in \code{DT} which correspond to the id, and direction columns.
#'
#' @inheritParams centroid_fusion
#' @inheritParams direction_group
#' @param window temporal window in unit of timegroup column generated with
#'   \code{group_times}, eg. \code{window = 4} corresponds to the 4 timegroups
#'   before and after the focal observation
#'
#' @return \code{edge_delay} returns the input \code{edges} appended with
#'   a 'dir_corr_delay' column indicating the temporal delay (in units of
#'   timegroups) at which ID1's direction of movement is most similar to
#'   ID2's direction of movement, within the temporal window defined.
#'
#' @export
#'
#' @family Edge-list generation
#'
#' @examples
#' # Load data.table
#' library(data.table)
#' \dontshow{data.table::setDTthreads(1)}
#'
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
#'
#' # Select only individuals A, B, C for this example
#' DT <- DT[ID %in% c('A', 'B', 'C')]
#'
#' # Cast the character column to POSIXct
#' DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
#'
#' # Temporal grouping
#' group_times(DT, datetime = 'datetime', threshold = '20 minutes')
#'
#' # Calculate direction
#' direction_step(
#'   DT = DT,
#'   id = 'ID',
#'   coords = c('X', 'Y'),
#'   projection = 32736
#' )
#'
#' # Distance based edge list generation
#' edges <- edge_dist(
#'   DT,
#'   threshold = 100,
#'   id = 'ID',
#'   coords = c('X', 'Y'),
#'   timegroup = 'timegroup',
#'   returnDist = TRUE,
#'   fillNA = FALSE
#' )
#'
#' # Generate dyad id
#' dyad_id(edges, id1 = 'ID1', id2 = 'ID2')
#'
#' # Generate fusion id
#' fusion_id(edges, threshold = 100)
#'
#' # Directional correlation delay
#' delay <- edge_delay(
#'   DT,
#'   edges,
#'   window = 3,
#'   id = 'ID'
#' )
#'
#' print(delay)
edge_delay <- function(
    DT,
    edges,
    window = NULL,
    id = NULL,
    direction = 'direction') {

  if (is.null(DT)) {
    stop('input DT required')
  }

  if (is.null(edges)) {
    stop('input edges required')
  }

  if (is.null(id)) {
    stop('id column name required')
  }

  check_cols_edges <- c('ID1', 'ID2', timegroup)
  if (any(!(check_cols_edges %in% colnames(edges)))) {
    stop(paste0(
      as.character(paste(setdiff(
        check_cols_edges,
        colnames(edges)
      ), collapse = ', ')),
      ' field(s) provided are not present in input DT'
    ))
  }

  check_cols_DT <- c(id, timegroup, direction)
  if (any(!(check_cols_DT %in% colnames(DT)
  ))) {
    stop(paste0(
      as.character(paste(setdiff(
        check_cols_DT,
        colnames(DT)
      ), collapse = ', ')),
      ' field(s) provided are not present in input DT'
    ))
  }

  if (is.null(window)) {
    stop('window is required')
  }

  if (!is.numeric(window)) {
    stop('window should be a numeric, in the units of timegroup')
  }
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
