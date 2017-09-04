#' Build Home Ranges for Individuals
#'
#' @param hrType Type of HR estimation, defaults to 'mcp'
#' @inheritParams BuildPts
#'
#' @return Home range polygons for each ID
#' @export
BuildHRs <- function(hrType = 'mcp', DT, projection, coordFields = c('EASTING', 'NORTHING'),
                     idField = 'ID', spPts = NULL){
  if(any(!(c(idField, coordFields) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }
  if(is.null(spPts)){
    if(is.null(DT)) stop("must provide either spPts or DT")
    spPts <- BuildPts(DT, projection, coordFields, idField)
  }
  if(hrType == 'mcp'){
      adehabitatHR::mcp(spPts, percent = 95)
  } #else if(hrType == '')


  # adehabitatHR::kerneloverlap(spPts, method = "UDOI", percent = 95, grid = 700)
}

# is a binary HR overlap actually interesting?
# TODO: add different HR types
