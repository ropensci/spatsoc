# spatsoc [working.title]


## TODO
* Title: Group animal location data by their spatial *and temporal* relationship
* Description...
* check about which are exported
* decide if we want to (**as a standard**) allow users to provide column names, or force them to provide things as required.
* similarly about time groups

## functions
### groups-locs
* flex chaining or not?

### groups_lines
* ideally, don't use mapview, foreach for building the lines..
* pull out the line building to a semi hidden function (that users can use to see the lines, else hidden)

### mean pairwise

### foverlaps
involves two steps: time overlap grouping and spatial grouping (as before)
therefore wrap as finding time groups then finding spatial groups.

* rounding is here (ask him?): https://github.com/jangorecki/data.table/blob/iunit/R/IPeriod.R

## refer to...

http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2011.00169.x/epdf

wildlifeDI(/TG?)

asnipe

tim paco methods ecology evo paper

https://stackoverflow.com/questions/10527072/using-data-table-package-inside-my-own-package
