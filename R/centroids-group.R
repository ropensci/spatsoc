#' Calculate Centroids For Each Group
#'
#' Append two columns indicating the centroid coordinates for each group to an
#' group by ID, coordinates data.table/frame.
#'
#' @inheritParams BuildPts
#' @param groupField The group field generated
#'
#'
#' @examples
CalcCentroids <- function(DT, coords, groupField){
  if(any(!(c(groupField, coords) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }
  DT[, .(centroidX = mean(coords[1], na.rm = TRUE),
         centroidY = mean(coords[2], na.rm = TRUE)),
     by = groupField]
}

# or is this Group Locations ?
# as in the centroid or the MCP or etc
# then stack MCPs to find hotspots
