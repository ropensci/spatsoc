#' Group Spatial
#'
#' @param type
#' @param bufferWidth
#'
#'
#' @return
#' @export
#'
#' @examples
GroupSpatial <- function(geom, type, bufferWidth){
  if(type == 'pts'){
    buffers <- rgeos::gBuffer(geom, width = bufferWidth, byid = FALSE)
    o <- sp::over(geom, sp::disaggregate(buffers))
    dt <- data.table::data.table(geom@coords, id = geom$id, spatialGroup = o)
  } else if(type)

    # 2  other methods..
}
