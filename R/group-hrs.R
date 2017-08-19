#' Group Overlapping Polygons
#'
#' Group individuals by polygon (eg: home ranges) overlap
#'
#' @inheritParams BuildPts
#'
#' @param hrType Type of HR estimation, defaults to 'mcp'
#' @param spPolys Alternatively, provide a SpatialPolygons object.
#'
#' @return Group by ID (by time) data.table
#' @export
#'
#' @examples
#' # Build the HRs for a set of locs by individuals using mcp,
#' # kernel density estimation...
#' data(locs)
#'
#' groups <- GroupHRs('mcp', locs, 50, projection = '+proj=utm +zone=21 ellps=WGS84',
#'                    idField = 'ID')
#'
#' # If you'd like to simply compare proportion or overlap of a set of polygons,
#' # ...
#' data(locsPolys)
#'
#' groups <- GroupHRs(spPolys = locsPolys)
GroupHRs <- function(hrType = 'mcp', dt, projection, coordFields = c('EASTING', 'NORTHING'), idField = 'ID',
                     spPolys = NULL){
  if(is.null(spPolys)){
    if(is.null(dt)) stop("must provide either spPolys or dt")
    spPolys <- BuildHRs(hrType, dt, projection, coordFields, idField)
  }

  unionPolys <- rgeos::gUnaryUnion(spPolys)
  o <- sp::over(spPolys, sp::disaggregate(unionPolys))
  dt <- data.table::data.table(id = names(o), group = o)
}

# TODO: optional proportion overlap
# TODO: by year
# TODO: add adehabitat to depends
