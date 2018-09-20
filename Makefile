### Makefile for spatsoc
# Alec Robitaille

# Website
../spatsoc.gitlab.io/public/index.html: DESCRIPTION
	Rscript --vanilla -e "pkgdown::build_site(pkg = '../spatsoc', lazy = FALSE)"

