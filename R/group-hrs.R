#' Group Overlapping Polygons (Home ranges)
#'
#' Group individuals by polygon overlap
#'
#' @inheritParams BuildPts
#'
#' @param hrType Type of HR estimation, defaults to 'mcp'
#' @param spPolys Alternatively, provide a SpatialPolygons object.
#'
#' @return
#' @export
#'
#' @examples
GroupHRs <- function(hrType = 'mcp', dt, crs, coordFields = c('EASTING', 'NORTHING'), idField = 'ID',
                     spPolys = NULL){
  if(is.null(spPolys)){
    if(is.null(dt)) stop("must provide either spPolys or dt")
    spPolys <- BuildHRs(hrType, dt, crs, coordFields, idField)
  }

  unionPolys <- rgeos::gUnaryUnion(spPolys)
  o <- sp::over(spPolys, sp::disaggregate(unionPolys))
  dt <- data.table::data.table(id = names(o), group = o)
}

# TODO: optional proportion overlap
# TODO: by year
# TODO: add adehabitat to depends
