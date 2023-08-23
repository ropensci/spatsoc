# v 0.2.1 (2023-08-23)

* fixed CRAN notes [PR 56](https://github.com/ropensci/spatsoc/pull/56)


# v 0.2.0 (2023-08-22)

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
  - added temporary package startup message until October 2023

  
# v 0.1.17 (2023-03-16)

* added a link to our `spatsoc` + `targets` workflow example
* changed the error and underlying check for `group_polys` from alphanumeric to
spaces in input DT's id column
* clarify timegroups are required for `group_pts`, `edge_nn` and `edge_dist` ([PR 46](https://github.com/ropensci/spatsoc/pull/46))
* fix potential mixup between a column named splitBy and splitBy arg ([PR 45](https://github.com/ropensci/spatsoc/pull/45))
* fix links in man ([PR 47](https://github.com/ropensci/spatsoc/pull/47))
* fix proportions outside 0-1 due to differences in default units from rgeos::gArea 
and polys@area ([PR 49](https://github.com/ropensci/spatsoc/pull/49))
* Add keyword internal by @maelle in https://github.com/ropensci/spatsoc/pull/40
* Rm unnecessary lines by @maelle in https://github.com/ropensci/spatsoc/pull/43


# v 0.1.16 (2021-03-23)
* added an option for `edge_dist` to handle threshold = NULL. If NULL, `edge_dist` will return all neighbours observed (eg. useful if one wanted to calculated mean nearest neighbour distance at each timegroup). 
* updated EPSG argument according to newest recommendations in tests, man and vignettes ([PR 38](https://github.com/ropensci/spatsoc/pull/38)
* removed expect_silent tests ([PR 37](https://github.com/ropensci/spatsoc/pull/37))
* switched CI for tests and code coverage to GitHub Actions ([PR 36](https://github.com/ropensci/spatsoc/pull/36))


# v 0.1.15 (2020-10-21)
* fix TZ=UTC data.table tests ([Issue 32](https://github.com/ropensci/spatsoc/issues/32))

# v 0.1.14 (2020-07-03)
* updated tests, man and vignettes following new handling of projections in sp ([PR 31](https://github.com/ropensci/spatsoc/pull/31), [R spatial information](https://r-spatial.org/r/2020/03/17/wkt.html))
* clarified explicit drop of NAs in dyadID in edge list vignette

# v 0.1.13 (2020-03-25)
* added `dyad_id` function for generating dyad IDs with edge functions ([PR 27](https://github.com/ropensci/spatsoc/pull/25))
* added a vignette describing `edge_dist`, `edge_nn` and `dyad_id` functions [here](https://docs.ropensci.org/spatsoc/articles/using-edge-and-dyad.html) ([PR 14](https://github.com/ropensci/spatsoc/pull/14))

# v 0.1.12 (2020-03-02)
* fixed `data.table` error in `edge_dist` and `edge_nn` ([PR 25](https://github.com/ropensci/spatsoc/pull/25))


# v 0.1.11 (2020-02-20)
* removed default NULL from 'timegroup' arguments in `group_pts`, `edge_dist` and `edge_nn` ([PR 24](https://github.com/ropensci/spatsoc/pull/24))


# v 0.1.10 (2019-06-06)
* added optional return of distance between individuals with `edge_dist` ([PR 19](https://github.com/ropensci/spatsoc/pull/19)) and `edge_nn` ([PR 21](https://github.com/ropensci/spatsoc/pull/21))


# v 0.1.9 (2019-05-14)
* fixed bug for randomizations type 'step' and 'daily' ([PR 13](https://github.com/ropensci/spatsoc/pull/13)). 
* clarified `SIMPLIFY=FALSE` in SNA vignette. 


# v 0.1.8 (2019-04-05)
* update [FAQ](https://docs.ropensci.org/spatsoc/articles/faq.html) and [Introduction to spatsoc](https://docs.ropensci.org/spatsoc/articles/intro-spatsoc.html) vignettes adding entries for edge list generating functions. 
* added edge list generating function `edge_nn` ([PR 11](https://github.com/ropensci/spatsoc/pull/12))
* added edge list generating function `edge_dist` ([PR 11](https://github.com/ropensci/spatsoc/pull/11))


# v 0.1.7 (2019-03-26)
* fix inconsistent blocks across years ([PR 10](https://github.com/ropensci/spatsoc/pull/10))
* update FAQ: remove old randomizations notes, clarify group_times block


# v 0.1.6 (2019-01-10)
* fix bug 'group_times misses nearest hour with mins threshold' ([#5](https://github.com/ropensci/spatsoc/issues/5) and [PR 6](https://github.com/ropensci/spatsoc/pull/6))

# v 0.1.5 (2018-12-04)
* update issue labels and contributing
* change over issue board location from GitLab to rOpenSci repository on GitHub
* added preprint CITATION
* added "https://" to `pkgdown` URL ([PR 1](https://github.com/ropensci/spatsoc/pull/1))

# v 0.1.4 (2018-10-26)
* fin [rOpenSci onboarding process](https://github.com/ropensci/software-review/issues/237)
* fixed bug couldn't provide percent to kernel type `build_polys` or `group_polys`([!3](https://gitlab.com/robitalec/spatsoc/-/merge_requests/3))


# v 0.1.3 
* added `get_gbi` to generate group by individual matrices for better integrating `spatsoc` in social network analysis workflows ([!2](https://gitlab.com/robitalec/spatsoc/-/merge_requests/2))


# v 0.1.2

* **major change to randomizations**: when `iterations = 1`, `randomizations` no longer returns the DT with appended columns. Regardless of the value of iterations, `randomizations` always returns observed rows followed by randomized rows in a long `data.table` ([!1](https://gitlab.com/robitalec/spatsoc/-/merge_requests/1)). 

# v 0.1.1 (2018-09-17)

* improvements to package, function documentation
* [FAQ](https://docs.ropensci.org/spatsoc/articles/faq.html) vignette added
* fixed `build_lines` ordering bug to ensure rows are ordered by date time when building lines
* added CODE_OF_CONDUCT.md and CONTRIBUTING.md
* [Using spatsoc in social network analysis](https://docs.ropensci.org/spatsoc/articles/using-in-sna.html) vignette added

# v 0.1.0 (2018-07-20)

## Initial release

* temporal grouping function: `group_times`
* spatial grouping functions: `group_pts`, `group_lines`, `group_polys`
* data-stream randomization function: `randomizations`
* spatial build functions: `build_lines`, `build_polys`
