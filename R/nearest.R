#' Proportion/Frequency Nearest Neighbors
#'
#' @inheritParams BuildPts
#' @param timeField (optional) timeField in the DT upon which the neighbors'
#'   distance will be compared
#' @param groupField (optional) groupField in the DT which can be used to group
#'   neighbors (eg: season, year, herd, known social groups, ...)
Nearest <- function(DT, timeField = NULL, groupField = NULL, proportions = FALSE, coordFields = c('EASTING', 'NORTHING'),
                    idField = 'ID'){
  if(any(!(c(timeField, groupField, coordFields) %in% colnames(DT)))){
    stop('some fields provided are not present in data.table provided/colnames(DT)')
  }

  FindNearest <- function(in.dt, coords, id){
    tree <- SearchTrees::createTree(in.dt[, ..coords])
    knn <- (SearchTrees::knnLookup(tree, newdat = in.dt[, ..coords], k = 2))
    return(list(ID = in.dt[, get(id)],
                neighbor = in.dt[, get(id)][knn[,2]]))
  }

  # check if there is a groupField variable
  if(is.null(groupField)){
    # if no timeField, calculate directly with all locs, else calc on by timeField
    if(is.null(timeField)){
      data.table::rbindlist(list(FindNearest(DT, coordFields, idField)))
    } else {
      d <- DT[, FindNearest(.SD, coordFields, idField), ##!!!!!!!!!!!!!!!
              by = timeField, .SDcols = c(coordFields, idField)]
      # optionally, return proportions or all matches
      if(!proportions){
        return(d)
      } else {
        d[, nTime := data.table::uniqueN(get(timeField)), by = ID]
        unique(d[, .(prop = .N / nTime), by = .(ID, neighbor)])
      }
    }
  } else {
  # if there is a groupField variable, but no timeField, return all pairs in each group
    if(is.null(timeField)){
      DT[, FindNearest(.SD, coordFields, idField),
              by = groupField]
    } else {
      # else, return either proportions or pairs by each group * time
      DT[, {d <- DT[, FindNearest(.SD, coordFields, idField),
                    by = timeField, .SDcols = c(coordFields, idField)]
            if(!proportions){
              d
            } else {
              d[, nTime := data.table::uniqueN(get(timeField)), by = ID]
              unique(d[, .(prop = .N / nTime), by = .(ID, neighbor)])
            }},
         by = groupField]
    }
  }
}


# TODO: eval(idField) or the like for naming the list with input idField
#       https://stackoverflow.com/questions/17169475/create-list-programmatically-with-tags-from-character-vector
