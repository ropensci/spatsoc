#' Euclidean Pairwise Distance
#'
#' @inheritParams BuildPts
#' @param datetime time column upon which individuals are compared. recommended
#'   to use a rounded time, see SOME LINK.
#'
#' @return mean pairwise distance data.table by ID and (optional) time field
PairwiseDist <- function(DT, datetime, coords = c('EASTING', 'NORTHING'), id = 'ID') {
  if(any(!(c(datetime, id, coords) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }
  if(is.null(datetime)) {
    warning('time column not provided - pairwise distance will be computed across all locs')

    names <- DT[, get(id)]

    distMatrix <- sp::spDists(as.matrix(DT[ , ..coords]),
                              longlat = FALSE)

    # Output the column means (average pairwise dist) + names
    data.table(meanDistance = colMeans(distMatrix),
               id = names)
  } else {
    DT[, {names <- .SD[, get(id)]
          distMatrix <- sp::spDists(as.matrix(.SD[ , ..coords]),
                                    longlat = FALSE)
          list(meanDistance = colMeans(distMatrix),
               id = names)},
       by = datetime]
  }
}

# TODO: check for if any names are equal, output warning
