#' Direction based edge-lists
#' @family Edge-list generation
edge_direction <- function(
    edges = NULL,
    DT = NULL,
    id = NULL,
    coords = NULL,
    projection = NULL,
    timegroup = 'timegroup') {

  if (is.null(DT)) {
    stop('input DT required')
  }

  if (is.null(edges)) {
    stop('input edges required')
  }

  if (is.null(id)) {
    stop('id column name required')
  }

  if (length(coords) != 2) {
    stop('coords requires a vector of column names for coordinates X and Y')
  }

  if (is.null(timegroup)) {
    stop('timegroup required')
  }

  if (any(!(
    c('dyadID', timegroup) %in% colnames(edges)
  ))) {
    stop(paste0(
      as.character(paste(setdiff(
        c('dyadID', timegroup),
        colnames(edges)
      ), collapse = ', ')),
      ' field(s) provided are not present in input DT'
    ))
  }

  if (any(!(
    c(id, coords, timegroup) %in% colnames(DT)
  ))) {
    stop(paste0(
      as.character(paste(setdiff(
        c(id, coords, timegroup),
        colnames(DT)
      ), collapse = ', ')),
      ' field(s) provided are not present in input DT'
    ))
  }

  if (any(!(DT[, vapply(.SD, is.numeric, TRUE), .SDcols = coords]))) {
    stop('coords must be numeric')
  }

  xcol <- data.table::first(coords)
  ycol <- data.table::last(coords)

  out_col <- 'direction_dyad'

  id1_coords <- paste0('id1_', coords)
  id2_coords <- paste0('id2_', coords)

  m <- merge(edges,
             DT[, .SD, .SDcols = c(coords, id, 'timegroup')],
             by.x = c('ID1', timegroup),
             by.y = c(id, timegroup),
             all.x = TRUE,
             sort = FALSE)
  data.table::setnames(m, coords, id1_coords)
  m <- merge(m,
             DT[, .SD, .SDcols = c(coords, id, 'timegroup')],
             by.x = c('ID2', timegroup),
             by.y = c(id, timegroup),
             all.x = TRUE,
             sort = FALSE)
  data.table::setnames(m, coords, id2_coords)

  if (out_col %in% colnames(m)) {
    message(paste(out_col, 'column will be overwritten by this function'))
    data.table::set(m, j = out_col, value = NULL)
  }


}
