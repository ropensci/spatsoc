# Decisions

> just a space to keep track of decisions to make or that have been made about code structure, package/function usage, etc.

## Decisions..

* for `BuildLines`: is byFields really just id + whatever.. is this one argument then?

by separating byFields and idField, we emphasize the requirement of idField and extra byFields 

----

* use @importFrom pkg fun to decrease cost of repeated function calls?




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
