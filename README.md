
# spatsoc

The spatsoc package detects spatial and temporal groups in GPS
relocations. It can be used to convert GPS relocations to
gambit-of-the-group format to build proximity-based social networks. In
addition, the randomizations function provides data-stream randomization
methods suitable for GPS data.

## Installation

``` r
drat::addRepo('LocalRepo', 'https://spatsoc.gitlab.io')
install.packages('spatsoc')
```

\#\#Using `drat`

``` r
drat::addRepo('LocalRepo', 'https://spatsoc.gitlab.io')
install.packages('spatsoc')
```

## Examples

``` r
library(spatsoc)
library(data.table)
DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
```

### `group_times`

``` r
library(spatsoc)
DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
DT[, datetime := as.POSIXct(datetime,
                            tz = 'UTC')]
```
