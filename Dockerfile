FROM rocker/r-base:latest
MAINTAINER Alec L. Robitaille

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libgeos-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    git \
  && install2.r --error \
    rgeos \
    sp \
    data.table \
    adehabitatHR \
    igraph \
    knitr \
    devtools \
    rmarkdown \
    testthat \
    roxygen2
