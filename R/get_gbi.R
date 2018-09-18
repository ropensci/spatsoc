#' Generate group by individual matrix
#'
#' @inheritParams group_pts
#' @param group Character string of group column (generated from spatsoc's spatial grouping functions)
#' @param type Character string indicating which spatial grouping function was used. \code{\link{group_pts}}: 'point', \code{\link{group_lines}}: 'line', \code{\link{group_polys}}: 'polygon'
#'
#' @export
#'
#' @seealso \code{\link{group_pts}} \code{\link{group_lines}} \code{\link{group_polys}}
#' @family Social network tools
#' @importFrom data.table dcast
#'
#'
#' @examples
#' # Load data.table
#' library(data.table)
#'
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
#'
#' # Cast the character column to POSIXct
#' DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
#' DT[, yr := year(datetime)]
#'
#' utm <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
#'
#' group_polys(DT, area = FALSE, hrType = 'mcp',
#'             hrParams = list(percent = 95),
#'             projection = utm, id = 'ID', coords = c('X', 'Y'),
#'             splitBy = 'yr')
#'
#' get_gbi(DT, 'group', 'ID')
#'
get_gbi <-
  function(DT = NULL,
           group = 'group',
           id = NULL) {

    if (is.null(DT)) {
      stop('input DT required')
    }

    if (is.null(group)) {
      stop('group field required')
    }

    if (is.null(id)) {
      stop('ID field required')
    }

    if (any(!(
      c(group, id) %in% colnames(DT)
    ))) {
      stop(paste0(
        as.character(paste(setdiff(
          c(group, id),
          colnames(DT)
        ), collapse = ', ')),
        ' field(s) provided are not present in input DT'
      ))
    }


    if (anyNA(DT[[group]])) {
      warning('DT contains NA(s) in group column')
    }

    uDT <-
      na.omit(
        unique(
          DT[, .SD, .SDcols = c(group, id)]),
        cols = group)

    d <-
      data.table::dcast(
        uDT,
        formula = reformulate(id, group),
        fun.aggregate = length,
        value.var = group
      )

    ids <- colnames(d)[!grepl(group, colnames(d))]

    gbi_df <- as.matrix(d[, .SD, .SDcols = ids])

    rownames(gbi_df) <- d[[group]]
    return(gbi_df)




  }
