#' Leadership in directional correlation delay
#'
#' Given the directional correlation delay, \code{leader_edge_delay}
#' calculates the mean directional correlation delay for individuals in a
#' group to identify leadership patterns.
#'
#' The function expects an edge list from \code{edge_delay} with columns
#' 'direction_delay' indicating the directional correlation delay between
#' individuals and 'direction_diff' indicating the unsigned difference in
#' movement directions at the temporal delay, columns 'ID1' and 'ID2' indicating
#' individuals and column 'dyadID' indicating the dyad.
#'
#' The \code{edge} must be a \code{data.table}. If your data is a
#' \code{data.frame}, you can convert it by reference using
#' \code{\link[data.table:setDT]{data.table::setDT}} or by reassigning using
#' \code{\link[data.table:data.table]{data.table::data.table}}.
#'
#' @return \code{leader_edge_delay} returns the input \code{edges} appended with
#'   a \code{mean_direction_delay_dyad} column indicating the mean directional
#'   correlation delay between ID1 and ID2 and a \code{mean_direction_delay}
#'   column indicating the mean directional correlation delay for each
#'   individual in 'ID1' column.
#'
#' @inheritParams edge_delay
#' @param threshold (optional) threshold difference in direction used to subset
#'   rows included in calculation of mean directional delay. eg.
#'   \code{threshold = 0.5} corresponds to only rows where direction_diff is
#'   less than 0.5. Expects that unit is radians, see \code{\link{edge_delay}}.
#' @param splitBy (optional) character string or vector of grouping column
#'   name(s) upon which the mean directional correlation delay will be
#'   calculated
#'
#' @export
#' @seealso \code{\link{edge_delay}}
#' @family Leadership functions
#' @family Direction functions
#'
#' @references
#' See examples of measuring leadership using the directional correlation
#' delay:
#'  * <https://doi.org/10.1016/j.anbehav.2013.07.005>
#'  * <https://doi.org/10.1073/pnas.1305552110>
#'  * <https://doi.org/10.1126/science.aap7781>
#'  * <https://doi.org/10.1111/jfb.15315>
#'  * <https://doi.org/10.1371/journal.pcbi.1003446>
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
#'   edges = edges,
#'   DT = DT,
#'   window = 3,
#'   id = 'ID'
#' )
#'
#' # Leadership from directional correlation delay
#' leadership <- leader_edge_delay(
#'   delay,
#'   threshold = 0.5
#' )
#' print(leadership)
leader_edge_delay <- function(
    edges = NULL,
    threshold = NULL,
    splitBy = NULL) {
  # Due to NSE notes
  . <- direction_diff <- direction_delay <- mean_direction_delay <-
    mean_direction_delay_dyad <- NULL

  if (is.null(edges)) {
    stop('input edges required')
  }

  check_cols <- c('direction_delay', 'direction_diff',
                  'ID1', 'ID2', splitBy)
  if (any(!(check_cols %in% colnames(edges)))) {
    stop(paste0(
      as.character(paste(setdiff(
        check_cols,
        colnames(edges)
      ), collapse = ', ')),
      ' field(s) provided are not present in input edges, did you use edge_delay?'
    ))
  }

  if (any(!(edges[, vapply(.SD, is.numeric, TRUE),
                  .SDcols = check_cols[seq.int(2)]]))) {
    stop('direction_delay and direction_diff must be numeric, did you use edge_delay?')
  }

  if (is.null(threshold)) {
    threshold <- Inf
  } else {
    if (!is.numeric(threshold)) {
      stop('threshold must be numeric, in the units of direction_diff (rad)')
    }
  }

  subset_threshold <- edges[direction_diff < threshold]

  out <- subset_threshold[,
    .(mean_direction_delay_dyad = mean(direction_delay, na.rm = TRUE)),
    by = c('ID1', 'ID2', 'dyadID', splitBy)]

  out[, mean_direction_delay := mean(mean_direction_delay_dyad, na.rm = TRUE),
      by = c('ID1', splitBy)]

  return(out[])
}
