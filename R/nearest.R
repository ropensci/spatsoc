#' Proportion/Frequency Nearest Neighbors
#'
#' @inheritParams BuildPts
#' @param datetime (optional) datetime in the DT upon which the neighbors'
#'   distance will be compared
#' @param groupField (optional) groupField in the DT which can be used to group
#'   neighbors (eg: season, year, herd, known social groups, ...)
Nearest <- function(DT, datetime = NULL, groupField = NULL, proportions = FALSE, coords = c('EASTING', 'NORTHING'),
                    id = 'ID'){
  if(any(!(c(datetime, groupField, coords) %in% colnames(DT)))){
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
    # if no datetime, calculate directly with all locs, else calc on by datetime
    if(is.null(datetime)){
      data.table::rbindlist(list(FindNearest(DT, coords, id)))
    } else {
      d <- DT[, FindNearest(.SD, coords, id), ##!!!!!!!!!!!!!!!
              by = datetime, .SDcols = c(coords, id)]
      # optionally, return proportions or all matches
      if(!proportions){
        return(d)
      } else {
        d[, nTime := data.table::uniqueN(get(datetime)), by = ID]
        unique(d[, .(prop = .N / nTime), by = .(ID, neighbor)])
      }
    }
  } else {
  # if there is a groupField variable, but no datetime, return all pairs in each group
    if(is.null(datetime)){
      DT[, FindNearest(.SD, coords, id),
              by = groupField]
    } else {
      # else, return either proportions or pairs by each group * time
      DT[, {d <- DT[, FindNearest(.SD, coords, id),
                    by = datetime, .SDcols = c(coords, id)]
            if(!proportions){
              d
            } else {
              d[, nTime := data.table::uniqueN(get(datetime)), by = ID]
              unique(d[, .(prop = .N / nTime), by = .(ID, neighbor)])
            }},
         by = groupField]
    }
  }
}


# TODO: eval(id) or the like for naming the list with input id
#       https://stackoverflow.com/questions/17169475/create-list-programmatically-with-tags-from-character-vector
