
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![](https://badges.ropensci.org/237_status.svg)](https://github.com/ropensci/onboarding/issues/237)
[![](https://img.shields.io/badge/devel%20version-0.1.8-blue.svg)](https://github.com/robitalec/spatsoc)
[![CRAN](https://www.r-pkg.org/badges/version/spatsoc)](https://cran.r-project.org/package=spatsoc)
[![codecov](https://codecov.io/gl/robit.a/spatsoc/branch/master/graph/badge.svg)](https://codecov.io/gl/robit.a/spatsoc)
[![pipeline
status](https://gitlab.com/robit.a/spatsoc/badges/master/pipeline.svg)](https://gitlab.com/robit.a/spatsoc/commits/master)

# spatsoc

spatsoc is an R package for detecting spatial and temporal groups in GPS
relocations. It can be used to convert GPS relocations to
gambit-of-the-group format to build proximity-based social networks with
grouping and edge-liste generating functions. In addition, the
`randomizations` function provides data-stream randomization methods
suitable for GPS data and the `get_gbi` function generates group by
individual matrices useful for building networks with
`asnipe::get_network`.

See below for installation and basic usage.

For more details, see the [blog
post](https://ropensci.org/blog/2018/12/04/spatsoc/) and vignettes:

  - [Introduction to
    spatsoc](https://spatsoc.gitlab.io/articles/intro-spatsoc.html)
  - [Frequently asked
    questions](https://spatsoc.gitlab.io/articles/faq.html)
  - [Using spatsoc in social network
    analysis](http://spatsoc.gitlab.io/articles/using-in-sna.html)

## Installation

Install the latest version with `remotes`.

``` r
remotes::install_github('ropensci/spatsoc')

# or CRAN
install.packages('spatsoc')
```

`spatsoc` depends on `rgeos` and requires
[GEOS](https://trac.osgeo.org/geos/) installed on the system.

  - Debian/Ubuntu: `apt-get install libgeos-dev`
  - Arch: `pacman -S geos`
  - Fedora: `dnf install geos geos-devel`
  - Mac: `brew install geos`
  - Windows: see [here](https://trac.osgeo.org/osgeo4w/)

## Usage

### Import package, read data

`spatsoc` expects a `data.table` for all of its functions. If you have a
`data.frame`, you can use `data.table::setDT()` to convert it by
reference. If your data is a text file (e.g.: CSV), you can use
`data.table::fread()` to import it as a `data.table`.

``` r
library(spatsoc)
library(data.table)
DT <- fread(system.file("extdata", "DT.csv", package = "spatsoc"))
DT[, datetime := as.POSIXct(datetime, tz = 'UTC')]
```

### Temporal grouping

#### group\_times

``` r
group_times(
  DT, 
  datetime = 'datetime', 
  threshold = '5 minutes'
)
```

### Spatial grouping

#### group\_pts

``` r
group_pts(
  DT,
  threshold = 5,
  id = 'ID',
  coords = c('X', 'Y'),
  timegroup = 'timegroup'
)
```

#### group\_lines

``` r
utm <-
  '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
  
group_times(
  DT, 
  datetime = 'datetime', 
  threshold = '1 day'
)

group_lines(
  DT,
  threshold = 50,
  projection = utm,
  id = 'ID',
  coords = c('X', 'Y'),
  timegroup = 'timegroup',
  sortBy = 'datetime'
)
```

#### group\_polys

``` r
utm <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

group_polys(
  DT,
  area = FALSE,
  'mcp',
  list(percent = 95),
  projection = utm,
  id = 'ID',
  coords = c('X', 'Y')
)
  
areaDT <- group_polys(
  DT,
  area = TRUE,
  'mcp',
  list(percent = 95),
  projection = utm,
  id = 'ID',
  coords = c('X', 'Y')
)
```

# Contributing

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

Development of `spatsoc` welcomes contribution of feature requests, bug
reports and suggested improvements through the [issue
board](https://github.com/ropensci/spatsoc/issues).

See details in
[CONTRIBUTING.md](CONTRIBUTING.md).

[![ropensci\_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
