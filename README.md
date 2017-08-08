# spatsoc [working.title]


## TODO
* Title: Group animal location data by their spatial *and temporal* relationship
* Description...
* check about which are exported
* decide if we want to (**as a standard**) allow users to provide column names, or force them to provide things as required. OR add a prep step
* similarly about time groups
* group polys or group HRs?
* output data.tables can go back to lists if they are run BY = ...
* look at moveBank other forms of data
* missing adehabitat
read about dt in functions/packages
* https://stackoverflow.com/questions/13756178/writings-functions-procedures-for-data-table-objects
* https://stackoverflow.com/questions/28078640/adding-new-columns-to-a-data-table-by-reference-within-a-function-not-always-wor
* https://stackoverflow.com/questions/30601332/data-table-assignment-by-reference-within-function
* https://stackoverflow.com/questions/8030452/pass-by-reference-operator-in-the-data-table-package-modifies-another-data
* can't use spatsoc, too similar to spatstat


* if ID is character vector, .. paste??


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
