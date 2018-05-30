# spatsoc [working.title]
Title: Group animal location data by their spatial and temporal relationship

https://github.com/Rdatatable/data.table/issues/2053

https://rdatatable.gitlab.io/data.table/library/data.table/doc/datatable-importing.html


## Style
* group polys or group HRs?
* can't use spatsoc, too similar to spatstat (what about `socspat`, `socialspatial` )
* (later) review all variable, function etc names for conflicts and for clarity/brevity
* do we prefer grp_lines or GroupLines

* group_points
* grp_pts
* GroupPts
* GroupPoints

R source code

* group-pts
* group_pts

**single return per function**

## Test data
https://www.datarepository.movebank.org/handle/10255/move.619
https://www.datarepository.movebank.org/handle/10255/move.609

 
## Tests

* readcsv 
* read_table


do we want to return vertices strictly for kernel?


Check that date column provided has more than one uniqueN and not more than 1million (ha)


## TODO
* check that all functions have proper is null DT and colnames present / it didn't break anything
* check for [] for DT and fix so outputs what we want
* what are the incompatibilities between randomization + grouping methods


* explain that returned objects are data.tables and could be converted back with `setDF`


## GroupPts
ID and distance are mandatory


## Vignettes
* highlight build blocks

### Functions
#### Global
* missing adehabitat, SearchTrees
* if ID is character vector, paste?? (as in ID, Year ---> AN1_2007)
* or: try   as Idate/asITime  pull hours + minutes. if minutes > 30, add 30 else keep hours. thatll round to closest hour
* create example data in xy loc form
* create example data in sp form for lines, points and multipolygons
* must check if provided columns are found in input data, otherwise non descript errors like type closure (since date col similar to date function)
* remove comments from functions
* function for get_group_by_individual speedup
* check improvements in performance given the switch to an integer time group vs posixct

#### Foverlaps
* build clusters
* output from clusters consistent (time start, end + xy)

### Man
* Description...
* check about which are exported
* in vignette descripe how to run within a data.table to update columns etc

### Decisions
* decide if we want to (**as a standard**) allow users to provide column names, or force them to provide things as required. OR add a prep step.
* similarly about time groups
* is it acceptable for a user to be **required** to provide a data.table? https://stackoverflow.com/questions/26069219/using-setdt-inside-a-function
* is the package going to follow suit with data.table's modify on reference or is it going to simply return columns? (or option(modByRef = TRUE))
* should we only return one or few columns so that the functions can integrate in dt[, grp := ...]
* use @importFrom pkg fun to decrease cost of repeated function callsMV917540R0


## To Read
### Build
* https://stackoverflow.com/questions/28078640/adding-new-columns-to-a-data-table-by-reference-within-a-function-not-always-wor
* https:/stackoverflow.com/questions/30601332/data-table-assignment-by-reference-within-function/
* https://stackoverflow.com/questions/8030452/pass-by-reference-operator-in-the-data-table-package-modifies-another-data
* https://stackoverflow.com/questions/10527072/using-data-table-package-inside-my-own-package



* https://github.com/Rdatatable/data.table/issues/2053
