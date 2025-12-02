# docker/app.R
library(shiny)
library(SpecimenCat)

# Ensure Shiny listens on all interfaces in the container
options(
  shiny.port = 3838,
  shiny.host = "0.0.0.0"
)

# Launch the main eLTER-IT catalogues app
elter_catalogues_app(default_tab = "samples")
