% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/centroid_fusion.R
\name{centroid_fusion}
\alias{centroid_fusion}
\title{Fusion centroid}
\usage{
centroid_fusion(
  edges = NULL,
  DT = NULL,
  id = NULL,
  coords = NULL,
  timegroup = "timegroup",
  na.rm = FALSE
)
}
\arguments{
\item{edges}{edge list generated generated by \code{edge_dist} or
\code{edge_nn}, with fusionID column generated by \code{fusion_id}}

\item{DT}{input data.table with timegroup column generated with
\code{group_times} matching the input data.table used to generate the edge
list with \code{edge_nn} or \code{edge_dist}}

\item{id}{character string of ID column name}

\item{coords}{character vector of X coordinate and Y coordinate column names.
Note: the order is assumed X followed by Y column names.}

\item{timegroup}{timegroup field in the DT within which the grouping will be
calculated}

\item{na.rm}{if NAs should be removed in calculating mean location, see
\code{rowMeans}}
}
\value{
\code{centroid_fusion} returns the input \code{edges} appended with
centroid columns for the X and Y coordinate columns.

These columns represents the centroid coordinate columns for each timestep
in a fusion event. The naming of these columns will correspond to the
provided coordinate column names prefixed with "centroid_".

Note: due to the merge required within this function, the output needs to
be reassigned unlike some other \code{spatsoc} functions like
\code{fusion_id} and \code{group_pts}.

A message is returned when centroid columns are already exists in the input
\code{edges}, because they will be overwritten.
}
\description{
\code{centroid_fusion} calculates the centroid (mean location) of each
timestep in fusion events. The function accepts an edge list of fusion events
identified by \code{fusion_id} from edge lists generated with
\code{edge_dist} and a \code{data.table} with relocation data appended with a
\code{timegroup} column from \code{group_times}. It is recommended to use the
argument \code{fillNA = FALSE} for \code{edge_dist} when using
\code{centroid_fusion} to avoid unnecessarily merging additional rows.
Relocation data should be in two columns representing the X and Y
coordinates.
}
\details{
The \code{edges} and \code{DT} must be \code{data.table}. If your data is a
\code{data.frame}, you can convert it by reference using
\code{\link[data.table:setDT]{data.table::setDT}} or by reassigning using
\code{\link[data.table:data.table]{data.table::data.table}}.

The \code{edges} and \code{DT} are internally merged in this function using
the columns \code{timegroup} (from \code{group_times}) and \code{ID1} and
\code{ID2} (in \code{edges}, from \code{dyad_id}) and \code{id} (in
\code{DT}). This function expects a \code{fusionID} present, generated with
the \code{fusion_id} function. The \code{timegroup} argument expects the
names of a column in \code{edges} which correspond to the timegroup column.
The \code{id}, \code{coords} and \code{timegroup} arguments expect the names
of a column in \code{DT} which correspond to the id, X and Y coordinates and
timegroup columns. The \code{na.rm} argument is passed to the \code{rowMeans}
function to control if NA values are removed before calculation.
}
\examples{
# Load data.table
library(data.table)
\dontshow{data.table::setDTthreads(1)}

# Read example data
DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))

# Cast the character column to POSIXct
DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]

# Temporal grouping
group_times(DT, datetime = 'datetime', threshold = '20 minutes')

# Edge list generation
edges <- edge_dist(
    DT,
    threshold = 100,
    id = 'ID',
    coords = c('X', 'Y'),
    timegroup = 'timegroup',
    returnDist = TRUE,
    fillNA = FALSE
  )

# Generate dyad id
dyad_id(edges, id1 = 'ID1', id2 = 'ID2')

# Generate fusion id
fusion_id(edges, threshold = 100)

# Calculate fusion centroid
centroids <- centroid_fusion(
  edges,
  DT,
  id = 'ID',
  coords = c('X', 'Y'),
  timegroup = 'timegroup', na.rm = TRUE
)

print(centroids)
}
\seealso{
\link{fusion_id} \link{edge_dist} \link{group_pts}

Other Centroid functions: 
\code{\link{centroid_dyad}()},
\code{\link{centroid_group}()},
\code{\link{direction_to_centroid}()},
\code{\link{distance_to_centroid}()}
}
\concept{Centroid functions}
