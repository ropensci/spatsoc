# Development version

New experimental functions:

* `get_geometry` helper function for setting up an input DT with a sfc geometry
column ([PR 117](https://github.com/ropensci/spatsoc/pull/117))


# spatsoc 0.2.11

New experimental functions:

* `edge_direction` function for calculating the direction between individuals ([PR 80](https://github.com/ropensci/spatsoc/pull/80))
* `edge_zones` function for calculating behavioural zones ([PR 112](https://github.com/ropensci/spatsoc/pull/112))

Fixes:

* Fix `centroid_dyad` and `centroid_fusion` return NA for centroids 
when dyadID / fusionID are NA ([PR 114](https://github.com/ropensci/spatsoc/pull/114))

Maintenance:

* Doc fixes including replacing all roxygen tags (eg. `\code{}`) with Markdown 
throughout package, and using consistent language across man pages
([#94](https://github.com/ropensci/spatsoc/issues/94), 
[PR 113](https://github.com/ropensci/spatsoc/pull/113))
* Used {flir} to fix some lints 
([PR 115](https://github.com/ropensci/spatsoc/pull/115))

# spatsoc 0.2.10

New experimental function:

* `edge_alignment` function for calculating directional alignment ([PR 111](https://github.com/ropensci/spatsoc/pull/111))

Fixes:

* fix missing pkg::function ([PR 110](https://github.com/ropensci/spatsoc/pull/110))

# spatsoc 0.2.9

Breaking changes:

* Bug where Cij != -Cji fixed in `edge_delay` , since order of individuals ID1/ID2 should not influence results 
([PR 106](https://github.com/ropensci/spatsoc/pull/106))
* Improve output colnames for edge_delay 
([PR 102](https://github.com/ropensci/spatsoc/pull/102))

New experimental function:

* `leader_edge_delay` function for calculating leadership with the directional
correlation delay ([PR 96](https://github.com/ropensci/spatsoc/pull/96))

Fixes: 

* Fix {units} version requirement ([PR 96](https://github.com/ropensci/spatsoc/pull/96), 
[PR 99](https://github.com/ropensci/spatsoc/pull/99))
* Fix use new GitHub actions ([PR 101](https://github.com/ropensci/spatsoc/pull/101))
* Fix missing keyword internal ([PR 98](https://github.com/ropensci/spatsoc/pull/98))
* Add test window size for edge_delay ([PR 103](https://github.com/ropensci/spatsoc/pull/103))
* Improve documentation of which functions use modify-by-reference 
([PR 109](https://github.com/ropensci/spatsoc/pull/109))

# spatsoc 0.2.8

New experimental function: 

* `edge_delay` function for calculating the directional correlation delay 
([PR 70](https://github.com/ropensci/spatsoc/pull/70))

Fixes: 

* fix bracket typo longlat `direction_step` ([PR 92](https://github.com/ropensci/spatsoc/pull/92))
* fix test expected direction of steps ([PR 93](https://github.com/ropensci/spatsoc/pull/93))
* fix GitHub actions: update to workflow versions, add manual install MacOS system dependencies ([PR 101](https://github.com/ropensci/spatsoc/pull/101))
* add dependency on {units} version 0.8-6 that corrects error in %% operator arithmetic ([PR 100](https://github.com/ropensci/spatsoc/pull/100))

# spatsoc 0.2.7

New experimental functions: 

* `leader_direction_group` function for calculating leadership defined as 
position within a spatiotemporal group along the mean direction of movement
([PR 66](https://github.com/ropensci/spatsoc/pull/66))
* `direction_to_leader` function for calculating the direction to the 
leader of each spatiotemporal group identified by `leader_direction_group` 
([PR 68](https://github.com/ropensci/spatsoc/pull/68))
* `distance_to_leader` function for calculating the distance to the 
leader of each spatiotemporal group identified by `leader_direction_group` 
([PR 68](https://github.com/ropensci/spatsoc/pull/68))

Fixes: 

* removed temporary startup message warning of changes related to 
R-spatial evolution


# spatsoc 0.2.6

New experimental functions: 

* `direction_polarization` function for calculating polarization of individual 
directions within spatiotemporal groups 
([PR 76](https://github.com/ropensci/spatsoc/pull/76))
* `direction_group` function for calculating mean group direction
([PR 91](https://github.com/ropensci/spatsoc/pull/91))
* `direction_to_centroid` function for calculating direction in radians from
each individual's position in a spatiotemporal group and the group's centroid, 
`distance_to_centroid` function for calculation the distance (and rank distance)
from each individual's position in a spatiotemporal group and the group's
centroid ([PR 74](https://github.com/ropensci/spatsoc/pull/74))
* `direction_step` function for calculating direction in radians between each
location ([PR 90](https://github.com/ropensci/spatsoc/pull/90))

# spatsoc 0.2.5

New experimental functions: 

* `centroid_fusion` function for calculation fusion centroids 
([PR 89](https://github.com/ropensci/spatsoc/pull/89))
* `centroid_group` function for calculating group centroids 
([PR 72](https://github.com/ropensci/spatsoc/pull/72)))

# spatsoc 0.2.4

New experimental function: 

* `fusion_id` function for flexibly identifying fission-fusion events 
([PR 78](https://github.com/ropensci/spatsoc/pull/78))
* improve tests of `fusion_id` with tests for expected number of output fusionIDs 
([PR 83](https://github.com/ropensci/spatsoc/pull/83))


# spatsoc 0.2.3

* Fix/replace igraph clusters with components 
([PR 61](https://github.com/ropensci/spatsoc/pull/61))
* Fix/warnings from merge args 
([PR 62](https://github.com/ropensci/spatsoc/pull/61))

# spatsoc 0.2.2 (2023-09-07)

* fixed CRAN notes about number of cores/parallel process/threads 
([PR 58](https://github.com/ropensci/spatsoc/pull/58))


# spatsoc 0.2.1 (2023-08-23)

* fixed CRAN notes ([PR 56](https://github.com/ropensci/spatsoc/pull/56))


# spatsoc 0.2.0 (2023-08-22)

* following [R-spatial evolution](https://r-spatial.org/r/2022/04/12/evolution.html),
removed dependencies on retired spatial packages (
[PR 50](https://github.com/ropensci/spatsoc/issues/50):
[PR 52](https://github.com/ropensci/spatsoc/issues/52),
[PR 53](https://github.com/ropensci/spatsoc/issues/53),
[PR 54](https://github.com/ropensci/spatsoc/issues/54)
[PR 55](https://github.com/ropensci/spatsoc/issues/55))
  - spatsoc now depends on `sf`, `units` instead of `rgeos` and `sp`
  - `build_lines` now returns an `sf` LINESTRING object
  - `build_polys` now returns an `sf` POLYGON/MULTIPOLYGON object
  - `group_lines` now accepts an input `sf` LINESTRING object (argument "sfLines") 
  and internally uses `sf::st_intersects`, `sf::st_buffer`, etc instead of `rgeos` functions
  - `group_polys` now accepts an input `sf` POLYGON/MULTIPOLYGON object (argument "sfPolys") 
  and internally uses `sf::st_intersects`, `sf::st_area`, etc instead of `rgeos` functions. 
  `group_polys` now returns area and proportion of overlap when `area = TRUE` with 
  respective units using the `units` package
  - tests, vignettes, manual updated
  - added temporary package startup message until November 2024

  
# spatsoc 0.1.17 (2023-03-16)

* added a link to our `spatsoc` + `targets` workflow example
* changed the error and underlying check for `group_polys` from alphanumeric to
spaces in input DT's id column
* clarify timegroups are required for `group_pts`, `edge_nn` and `edge_dist` 
([PR 46](https://github.com/ropensci/spatsoc/pull/46))
* fix potential mixup between a column named splitBy and splitBy arg 
([PR 45](https://github.com/ropensci/spatsoc/pull/45))
* fix links in man ([PR 47](https://github.com/ropensci/spatsoc/pull/47))
* fix proportions outside 0-1 due to differences in default units from rgeos::gArea 
and polys@area ([PR 49](https://github.com/ropensci/spatsoc/pull/49))
* Add keyword internal by @maelle 
([PR 40](https://github.com/ropensci/spatsoc/pull/40))
* Rm unnecessary lines by @maelle 
([PR 43](https://github.com/ropensci/spatsoc/pull/43))


# spatsoc 0.1.16 (2021-03-23)

* added an option for `edge_dist` to handle threshold = NULL. If NULL, `edge_dist` will return all neighbours observed (eg. useful if one wanted to calculated mean nearest neighbour distance at each timegroup). 
* updated EPSG argument according to newest recommendations in tests, man and vignettes 
([PR 38](https://github.com/ropensci/spatsoc/pull/38)
* removed expect_silent tests 
([PR 37](https://github.com/ropensci/spatsoc/pull/37))
* switched CI for tests and code coverage to GitHub Actions 
([PR 36](https://github.com/ropensci/spatsoc/pull/36))


# spatsoc 0.1.15 (2020-10-21)

* fix TZ=UTC data.table tests ([Issue 32](https://github.com/ropensci/spatsoc/issues/32))

# spatsoc 0.1.14 (2020-07-03)

* updated tests, man and vignettes following new handling of projections in sp 
([PR 31](https://github.com/ropensci/spatsoc/pull/31), 
[R spatial information](https://r-spatial.org/r/2020/03/17/wkt.html))
* clarified explicit drop of NAs in dyadID in edge list vignette

# spatsoc 0.1.13 (2020-03-25)

* added `dyad_id` function for generating dyad IDs with edge functions 
([PR 27](https://github.com/ropensci/spatsoc/pull/25))
* added a vignette describing `edge_dist`, `edge_nn` and `dyad_id` functions [here](https://docs.ropensci.org/spatsoc/articles/using-edge-and-dyad.html) 
([PR 14](https://github.com/ropensci/spatsoc/pull/14))

# spatsoc 0.1.12 (2020-03-02)

* fixed `data.table` error in `edge_dist` and `edge_nn` 
([PR 25](https://github.com/ropensci/spatsoc/pull/25))


# spatsoc 0.1.11 (2020-02-20)

* removed default NULL from 'timegroup' arguments in `group_pts`, `edge_dist` 
and `edge_nn` ([PR 24](https://github.com/ropensci/spatsoc/pull/24))


# spatsoc 0.1.10 (2019-06-06)

* added optional return of distance between individuals with `edge_dist`
([PR 19](https://github.com/ropensci/spatsoc/pull/19)) and `edge_nn`
([PR 21](https://github.com/ropensci/spatsoc/pull/21))


# spatsoc 0.1.9 (2019-05-14)

* fixed bug for randomizations type 'step' and 'daily'
([PR 13](https://github.com/ropensci/spatsoc/pull/13)). 
* clarified `SIMPLIFY=FALSE` in SNA vignette. 


# spatsoc 0.1.8 (2019-04-05)

* update [FAQ](https://docs.ropensci.org/spatsoc/articles/faq.html) and
[Introduction to spatsoc](https://docs.ropensci.org/spatsoc/articles/intro.html) 
vignettes adding entries for edge list generating functions. 
* added edge list generating function `edge_nn` 
([PR 11](https://github.com/ropensci/spatsoc/pull/12))
* added edge list generating function `edge_dist` 
([PR 11](https://github.com/ropensci/spatsoc/pull/11))


# spatsoc 0.1.7 (2019-03-26)

* fix inconsistent blocks across years 
([PR 10](https://github.com/ropensci/spatsoc/pull/10))
* update FAQ: remove old randomizations notes, clarify group_times block


# spatsoc 0.1.6 (2019-01-10)

* fix bug 'group_times misses nearest hour with mins threshold' 
([#5](https://github.com/ropensci/spatsoc/issues/5) and 
[PR 6](https://github.com/ropensci/spatsoc/pull/6))

# spatsoc 0.1.5 (2018-12-04)

* update issue labels and contributing
* change over issue board location from GitLab to rOpenSci repository on GitHub
* added preprint CITATION
* added "https://" to `pkgdown` URL 
([PR 1](https://github.com/ropensci/spatsoc/pull/1))

# spatsoc 0.1.4 (2018-10-26)

* fin [rOpenSci onboarding process](https://github.com/ropensci/software-review/issues/237)
* fixed bug couldn't provide percent to kernel type `build_polys` or
`group_polys`([!3](https://gitlab.com/robitalec/spatsoc/-/merge_requests/3))


# spatsoc 0.1.3

* added `get_gbi` to generate group by individual matrices for better 
integrating `spatsoc` in social network analysis workflows
([!2](https://gitlab.com/robitalec/spatsoc/-/merge_requests/2))


# spatsoc 0.1.2

* **major change to randomizations**: when `iterations = 1`, 
`randomizations` no longer returns the DT with appended columns. 
Regardless of the value of iterations, `randomizations` always 
returns observed rows followed by randomized rows in a long `data.table`
([!1](https://gitlab.com/robitalec/spatsoc/-/merge_requests/1)). 

# spatsoc 0.1.1 (2018-09-17)

* improvements to package, function documentation
* [FAQ](https://docs.ropensci.org/spatsoc/articles/faq.html) vignette added
* fixed `build_lines` ordering bug to ensure rows are ordered by date time when building lines
* added CODE_OF_CONDUCT.md and CONTRIBUTING.md
* [Using spatsoc in social network analysis](https://docs.ropensci.org/spatsoc/articles/using-in-sna.html) vignette added

# spatsoc 0.1.0 (2018-07-20)

## Initial release

* temporal grouping function: `group_times`
* spatial grouping functions: `group_pts`, `group_lines`, `group_polys`
* data-stream randomization function: `randomizations`
* spatial build functions: `build_lines`, `build_polys`
