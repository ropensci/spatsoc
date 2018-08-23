#' spatsoc
#'
#' spatsoc is an R package for detecting spatial and temporal groups in GPS relocations. It can be used to convert GPS relocations to gambit-of-the-group format to build proximity-based social networks. In addition, the randomization function provides data-stream randomization methods suitable for GPS data.
#'
#'
#' The spatsoc package provides one temporal grouping function:
#'
#' \itemize{
#'   \item \code{\link{group_times}}
#' }
#' three spatial grouping functions:
#' \itemize{
#'   \item \code{\link{group_pts}}
#'   \item \code{\link{group_lines}}
#'   \item \code{\link{group_polys}}
#' }
#'
#' and one social network data-stream randomization function:
#' \itemize{
#'   \item \code{\link{randomizations}}
#' }
#'
#' @docType package
#' @name spatsoc
#' @aliases spatsoc-package
"_PACKAGE"
