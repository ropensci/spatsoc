# Decisions

> just a space to keep track of decisions to make or that have been made about code structure, package/function usage, etc.

## Decisions..

* can't use spatsoc, too similar to spatstat (what about `socspat`, `socialspatial` )

----

* group polys or group HRs?


----

* alternative to returning a withinGroup where locs < 2 ??
with withinGroup = NA

Column 2 of result for group 7 is type 'logical' but expecting type 'integer'. Column types must be consistent for each group.

----

* handle niche/weird time thresholds

if 90 minutes, group by 30 + hour?

1 day? 24 hour rounded to next day? WEIRD CASES

if 24 hours, change to one day?... but nearest day


----


* use set(DT, j = colnames(dtm), value = dtm) instead of DT[, (colnames(dtm)) := dtm][]

https://github.com/Rdatatable/data.table/issues/2788

----

* warn that date column provided has more than one uniqueN and not more than 1million (ha)/ warn if timegroup not provided, GroupTimes/slow

low: 1?
high: uniqueN <= half nrows 

----

* what are the incompatibilities between randomization + grouping methods

----

* SearchTrees, adehabitatHR missing. Is Nearest a necessary function in the initial package publishing?

----

* for `BuildLines`: is byFields really just id + whatever.. is this one argument then?

by separating byFields and idField, we emphasize the requirement of idField and extra byFields 

----

* do we want to return vertices strictly for kernel?

----

* use @importFrom pkg fun to decrease cost of repeated function calls?

----

* timegroup as returned variable's name?

----

* check if polar coordinates are provided?

----

* for `GroupTimes`: return days, minutes, etc?

----

* (later) review all variable, function etc names for conflicts and for clarity/brevity


## History

* decide if we want to (as a standard) allow users to provide column names, or force them to provide things as required. OR add a prep step.

**provide column names**

----

* is the package going to follow suit with data.table's modify on reference or is it going to simply return columns? (or option(modByRef = TRUE))

**modify by reference**

----

* is it acceptable for a user to be **required** to provide a data.table? https://stackoverflow.com/questions/26069219/using-setdt-inside-a-function

**yes**, since `setDT` is a simple function to use and describe to a user.. 

----

* should we only return one or few columns so that the functions can integrate in dt[, grp := ...]

**this is a given now, columns added directly**

----

* if ID is character vector, paste?? (as in ID, Year ---> AN1_2007)

**provide byFields/groupFields**

----

* create example data in sp form for lines, points and multipolygons

**not necessary**

----

* must check if provided columns are found in input data, otherwise non descript errors like type closure (since date col similar to date function)

**columns checks + tests written**


----

* does it chain?

**yes**

```r
GroupTimes(Dt, timeField = 'datetime', threshold = '3 hour')
distThreshold <- 500
GroupPts(Dt, distance = distThreshold, timeGroup = 'timegroup',
         coordFields = c('X', 'Y'), idField = 'ID')
Dt[, nByGroup := .N, by = group]
Dt[nByGroup > 1, maxDist := max(dist(cbind(X,Y))), by = group]

Dt[maxDist > distThreshold]
dstMtrx <- as.matrix(Dt[maxDist > distThreshold, dist(cbind(X,Y))])
igraph::clusters(igraph::graph_from_adjacency_matrix(dstMtrx < distThreshold))$membership

ggplot(Dt[maxDist > distThreshold]) +
  geom_point(aes(X, Y, color = ID, shape = factor(group))) +
  facet_wrap(~yr)
```