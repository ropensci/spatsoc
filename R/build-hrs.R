#' Build Home Ranges for Individuals
#'
#' @param hrType Type of HR estimation either 'mcp' or 'kernel'
#' @param hrParams List of named arguments to be passed to adehabitatHR functions as determined by hrType
#' @inheritParams BuildPts
#'
#' @return Home range polygons for each ID
#' @export
BuildHRs <- function(DT = NULL,
                     projection = NULL,
                     hrType = NULL,
                     hrParams = NULL,
                     coordFields = NULL,
                     idField = NULL,
                     spPts = NULL) {
  if(is.null(spPts)){
    if(is.null(DT)){
      stop("must provide either spPts or DT")
    }
    if(any(!(c(idField, coordFields) %in% colnames(DT)))){
      stop('some fields provided are not present in data.table provided/colnames(DT)')
    }
    spPts <- BuildPts(DT, projection, coordFields, idField)
  }
  hrParams$xy <- spPts
  if(hrType == 'mcp'){
    do.call(adehabitatHR::mcp, hrParams)
  } else if(hrType == 'kernel') {
    adehabitatHR::getverticeshr(
      do.call(adehabitatHR::kernelUD, hrParams))
  }
}


