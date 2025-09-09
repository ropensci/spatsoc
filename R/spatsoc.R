#' spatsoc
#'
#' spatsoc is an R package for detecting spatial and temporal groups in GPS
#' relocations. It can be used to convert GPS relocations to gambit-of-the-group
#' format to build proximity-based social networks. In addition, the
#' randomization function provides data-stream randomization methods suitable
#' for GPS data.
#'
#'
#' The spatsoc package provides one temporal grouping function:
#'
#' \itemize{ \item \code{group_times} } three spatial grouping functions:
#' \itemize{ \item \code{group_pts} \item \code{group_lines} \item
#' \code{group_polys} }
#'
#' two edge-list generating functions:
#'
#' \itemize{ \item \code{edge_dist} \item \code{edge_nn} }
#'
#' and two social network functions: \itemize{ \item
#' \code{randomizations} \item \code{get_gbi} }
#'
#' @docType package
#' @name spatsoc
#' @aliases spatsoc-package
#' @keywords internal
"_PACKAGE"
