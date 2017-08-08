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
* can't use spatsoc, too similar to spatstat
* if ID is character vector, paste?? (as in ID, Year ---> AN1_2007)


## To Read (build)
read about dt in functions/packages
* wowowow: https://stackoverflow.com/questions/25898162/data-table-anonymous-function-in-j
* suppressing intermediate... http://brooksandrew.github.io/simpleblog/articles/advanced-data-table/
* https://stackoverflow.com/questions/28078640/adding-new-columns-to-a-data-table-by-reference-within-a-function-not-always-wor
* https://stackoverflow.com/questions/30601332/data-table-assignment-by-reference-within-function
* https://stackoverflow.com/questions/8030452/pass-by-reference-operator-in-the-data-table-package-modifies-another-data


## Functions
### GroupPts
* flex chaining or not?
* `list(split(spPts@coords, c(col(spPts@coords)))` compare this to `as.list(as.data.frame)` for speed

### GroupLines
* ideally, don't use mapview, foreach for building the lines..

### GroupHRs/Polys
* proportional overlap?

### PairwiseDist
* only mean? or flexible stat?

### Foverlaps
* build clusters
* output from clusters consistent (time start, end + xy)
* rounding is here (ask him?): https://github.com/jangorecki/data.table/blob/iunit/R/IPeriod.R


## To Read (papers/packages)
* http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2011.00169.x/epdf
* wildlifeDI(/TG?)
* asnipe
* tim paco methods ecology evo paper

https://stackoverflow.com/questions/10527072/using-data-table-package-inside-my-own-package
