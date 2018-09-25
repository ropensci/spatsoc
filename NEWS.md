# v 0.1.4
## Development version


# v 0.1.3
* added `get_gbi` to generate group by individual matrices for better integrating `spatsoc` in social network analysis workflows


# v 0.1.2

* **major change to randomizations**: when `iterations = 1`, `randomizations` no longer returns the DT with appended columns. Regardless of the value of iterations, `randomizations` always returns observed rows followed by randomized rows in a long `data.table`. 

# v 0.1.1

* improvements to package, function documentation
* [FAQ](https://spatsoc.gitlab.io/articles/faq.html) vignette added
* fixed `build_lines` ordering bug to ensure rows are ordered by date time when building lines
* added CODE_OF_CONDUCT.md and CONTRIBUTING.md
* [Using spatsoc in social network analysis](https://spatsoc.gitlab.io/articles/using-in-sna.html) vignette added

# v 0.1.0 

## Initial release

* temporal grouping function: `group_times`
* spatial grouping functions: `group_pts`, `group_lines`, `group_polys`
* data-stream randomization function: `randomizations`
* spatial build functions: `build_lines`, `build_polys`
