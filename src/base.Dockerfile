# This makes the base biotea image. The tag is cmalabscience/biotea-base:...
# Build context is irrelevant.

FROM rocker/tidyverse:4.1.3

# Install necessary libs to compile R packages
# Some packages are not available from rocker, so I install as many deps as
# possible here.
RUN apt update && apt install -y --no-install-recommends \
    libgmp3-dev libmpfr-dev libcurl4-openssl-dev libssl-dev \
    libxml2-dev proj-bin libproj-dev libcairo2-dev libxt-dev libharfbuzz-dev \
    libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    libbz2-dev

# Install necessary R packages
# Disable threading for preprocessCore since it is bugged rigth now
RUN echo "options(configure.args = list(preprocessCore = '--disable-threading'))" >> /usr/local/lib/R/etc/Rprofile.site && \
  install2.r -n $(nproc --all) \
    progress yaml statmod VennDiagram gplots UpSetR testthat BiocManager jsonlite && \
  /usr/local/lib/R/site-library/littler/examples/installBioc.r \
    preprocessCore PCAtools limma RankProd oligo affycoretools EnhancedVolcano
