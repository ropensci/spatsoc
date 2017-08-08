#' Proportion/Frequency Nearest Neighbors
#'
#' @inheritParams BuildPts
#' @param spPts Alternatively, provide a SpatialPointsDataFrame created with the
#'   sp package. If a spPts object is provided, groups cannot be calculated by
#' @param timeField Time field in the dt upon which the spatial grouping will be
#'   calculated
#' @return
#' @export
#'
#' @examples
Nearest <- function(dt, timeField = NULL, crs, coordFields = c('EASTING', 'NORTHING'),
                    idField = 'ID', spPts = NULL){
  if(is.null(timeField)){
    if(is.null(spPts)){
      if(is.null(dt)) stop("must provide either pts or dt")
      spPts <- BuildPts(dt, crs, coordFields, idField)
      rowIDs <- data.table::data.table(dt[, ..idField])[, r := .I]
    }
    tree <- SearchTrees::createTree(spPts@coords)
    knn <- data.table::data.table(SearchTrees::knnLookup(tree, newdat = spPts@coords, k = 2))

    setnames(knn, c('id', 'neighbor'))

    merge(
      merge(rowIDs, knn, by.x = 'r', by.y = 'id'),
      rowIDs,
      by.x = 'neighbor', by.y = 'r')
  } else {
    if(!is.null(spPts)) stop("if providing a spPts, cannot provide a time field")
    dt[, {spPts <- BuildPts(.SD, crs, coordFields, idField)
          tree <- SearchTrees::createTree(spPts@coords)
          d <- data.table::data.table(SearchTrees::knnLookup(tree, newdat = spPts@coords, k = 2),
                                      id = get(idField))
          d[, neighbor := d[order(match(V1, V2)), id]]},
       by = timeField, .SDcols = c(coordFields, idField)][, c('V1', 'V2') := NULL]
  }
}

# TODO: check + clean the merge merge
