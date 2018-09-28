# Latest R with dependencies installed

FROM rocker/r-ver:latest
MAINTAINER Alec L. Robitaille

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libgeos-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    zlib1g-dev \
    qpdf \
    git \
  && install2.r --error \
    rgeos \
    sp \
    data.table \
    adehabitatHR \
    igraph \
    asnipe \
    knitr \
    devtools \
    rmarkdown \
    testthat \
    roxygen2 \
    covr
