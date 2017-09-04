#' Calculate Centroids For Each Group
#'
#' Append two columns indicating the centroid coordinates for each group to an
#' group by ID, coordinates data.table/frame.
#'
#' @inheritParams BuildPts
#' @param groupField The group field generated
#'
#' @export
#' @examples
CalcCentroids <- function(DT, coordFields, groupField){
  if(any(!(c(groupField, coordFields) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }
  DT[, .(centroidX = mean(coordFields[1], na.rm = TRUE),
         centroidY = mean(coordFields[2], na.rm = TRUE)),
     by = groupField]
}

# or is this Group Locations ?
# as in the centroid or the MCP or etc
# then stack MCPs to find hotspots
