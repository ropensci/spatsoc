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
CalcCentroids <- function(dt, coordFields, groupField){
  dt[, .(meanX = mean(coordFields[1], na.rm = TRUE),
         meanY = mean(coordFields[2], na.rm = TRUE)),
     by = groupField]
}


# or is this Group Locations ?
# as in the centroid or the MCP or etc
# then stack MCPs to find hotspots
