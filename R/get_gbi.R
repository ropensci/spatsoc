#' Generate group by individual matrix
#'
#' inheritParams group_pts
#' param
#'
#' @export
#'
#' @seealso \code{\link{group_pts}} \code{\link{group_lines}} \code{\link{group_polys}}
#' family


#' importFrom

#' examples
get_gbi <-
  function(DT = NULL,
           group = 'group',
           id = NULL,
           type = NULL) {

    if (is.null(DT)) {
      stop('input DT required')
    }

    if (is.null(group)) {
      stop('group field required')
    }

    if (is.null(id)) {
      stop('ID field required')
    }

  }
