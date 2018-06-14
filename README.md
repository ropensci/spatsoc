# spatsoc [working.title]
Title: Group animal location data by their spatial and temporal relationship

## Using `drat`

```r
# Author
# build tar with `R CMD build spatsoc`
drat::insertPackage('~/Documents/Local-git/spatsoc_0.0.0.9103.tar.gz',
'~/Documents/Local-git/spatsoc.gitlab.io/static')

# User
drat::addRepo('LocalRepo', 'spatsoc.gitlab.io')
install.packages('spatsoc')
```

## Test data
https://www.datarepository.movebank.org/handle/10255/move.619
https://www.datarepository.movebank.org/handle/10255/move.609

##
### Data.table awareness
* https://stackoverflow.com/questions/28078640/adding-new-columns-to-a-data-table-by-reference-within-a-function-not-always-wor
* https:/stackoverflow.com/questions/30601332/data-table-assignment-by-reference-within-function/
* https://stackoverflow.com/questions/8030452/pass-by-reference-operator-in-the-data-table-package-modifies-another-data
* https://stackoverflow.com/questions/10527072/using-data-table-package-inside-my-own-package
* https://github.com/Rdatatable/data.table/issues/2053
* https://rdatatable.gitlab.io/data.table/library/data.table/doc/datatable-importing.html
