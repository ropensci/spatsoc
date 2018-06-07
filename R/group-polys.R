#' Group Overlapping Polygons
#'
#' Group individuals by polygon (eg: home ranges) overlap
#'
#' @inheritParams BuildPts
#' @inheritParams GroupPts
#' @param area boolean indicating either returning area of overlap
#'                   of polygons or simply group
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
#' groups <- GroupPolys('mcp', locs, 50, projection = '+proj=utm +zone=21 ellps=WGS84',
#'                    idField = 'ID')
#'
#' # If you'd like to simply compare proportion or overlap of a set of polygons,
#' # ...
#' data(locsPolys)
#'
#' groups <- GroupPolys(spPolys = locsPolys)
GroupPolys <-
  function(area = FALSE,
           hrType = 'mcp',
           hrParams = NULL,
           DT = NULL,
           projection = NULL,
           coordFields = c('EASTING', 'NORTHING'),
           idField = 'ID',
           spPolys = NULL) {
    if (is.null(spPolys)) {
      if (is.null(DT)) {
        stop("must provide either spPolys or DT")
      }
      if (any(!(c(idField, coordFields) %in% colnames(DT)))) {
        stop('some fields provided are not present in data.table provided/colnames(DT)')
      }
      spPolys <-
        BuildHRs(hrType, hrParams, DT, projection, coordFields, idField)
    }

    if (area) {
      inters <- rgeos::gIntersection(spPolys, spPolys, byid = TRUE)

      data.table::data.table(area = sapply(
        inters@polygons,
        FUN = function(x) {
          slot(x, 'area')
        }
      ))[,
         c('ID1', 'ID2') := data.table::tstrsplit(sapply(
           inters@polygons,
           FUN = function(x) {
             slot(x, 'ID')
           }
         ),
         ' ',
         type.convert = TRUE)][]
      # this is susceptible to error if ID field provided has spaces
    } else {
      unionPolys <- rgeos::gUnaryUnion(spPolys)
      ovr <- sp::over(spPolys, sp::disaggregate(unionPolys))
      data.table::setnames(data.table::data.table(names(ovr),
                                                  ovr),
                           c(idField, 'group'))[]
    }

  }

# TODO: optional proportion overlap
# TODO: by year
# TODO: add adehabitat to depends



# if time group null
# build hrs
# over

# else if time group not null
# DT[, {

#}, by = group]

# then over within null etc
