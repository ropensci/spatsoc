## v 0.1.16
See NEWS.md for all updates. 

* added an option for `edge_dist` to handle threshold = NULL. If NULL, `edge_dist` will return all neighbours observed (eg. useful if one wanted to calculated mean nearest neighbour distance at each timegroup). 
* updated EPSG argument according to newest recommendations in tests, man and vignettes ([PR 38](https://github.com/ropensci/spatsoc/pull/38)
* removed expect_silent tests ([PR 37](https://github.com/ropensci/spatsoc/pull/37))
* switched CI for tests and code coverage to GitHub Actions ([PR 36](https://github.com/ropensci/spatsoc/pull/36))

## Test environments
* windows-latest (release) 
* macOS-latest (release)
* ubuntu-20.04 (release)
* ubuntu-20.04 (devel)
* ubuntu 16.04 (3.5)

## R CMD check results

There were no ERRORs, WARNING or NOTES
