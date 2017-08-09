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
                    idField = 'ID'){
  if(is.null(timeField)){
    # rowIDs <- dt[, .(get(idField), .I)]
    # tree <- SearchTrees::createTree(dt[, ..coordFields])
    # knn <- SearchTrees::knnLookup(tree, newdat = dt[, ..coordFields], k = 2)
    #
    # data.table::data.table(ID = dt[, get(idField)],
    #                        neighbor = rowIDs[order(match(I, knn[,2])), get(idField)])
    #
    #
    rowIDs <- dt[, .(id = get(idField), .I)]
    tree <- SearchTrees::createTree(dt[, ..coordFields])
    knn <- (SearchTrees::knnLookup(tree, newdat = dt[, ..coordFields], k = 2))

    data.table::data.table(ID = rowIDs[, id],
                           neighbor = rowIDs[order(match(I, knn[, 2])), id])
  } else {
    dt[, {
          tree <- SearchTrees::createTree(spPts@coords)
          d <- data.table::data.table(SearchTrees::knnLookup(tree, newdat = spPts@coords, k = 2),
                                      id = get(idField))
          d[, neighbor := d[order(match(V1, V2)), id]]},
       by = timeField, .SDcols = c(coordFields, idField)][, c('V1', 'V2') := NULL]
  }
}

# TODO: check + clean the merge merge
