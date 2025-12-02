FROM rocker/shiny:4.4.0

## ---- System libraries for sf, xml, curl, etc. ----
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

## ---- R packages needed by the app / API ----
RUN R -e "install.packages( \
    c('remotes','shinyvalidate','shinyjs','plumber', \
      'httr','httr2','callr','glue'), \
    repos = 'https://cloud.r-project.org' \
  )"

## Install SpecimenCat from GitHub (adjust org/repo if needed)
RUN R -e "remotes::install_github('ECJLTer/SpecimenCat')"

## ---- Copy Shiny app entrypoint ----
# Shiny Server serves apps from /srv/shiny-server
RUN mkdir -p /srv/shiny-server/specimen-cat
COPY app.R /srv/shiny-server/specimen-cat/app.R

## (Optional) change owner to shiny user
RUN chown -R shiny:shiny /srv/shiny-server

## Expose the Shiny port
EXPOSE 3838

## Start Shiny Server
CMD [\"/usr/bin/shiny-server\"]