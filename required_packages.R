required_packages <- c(
  "tidyverse",
  "rvest",
  "installr",
  "data.table",
  "janitor",
  "lubridate",
  "stringdist",
  "treemapify",
  "plotly"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(new_packages)) {
  install.packages(new_packages)
}

lapply(required_packages, require, character.only = TRUE)
