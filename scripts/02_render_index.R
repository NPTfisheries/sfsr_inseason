# Purpose: Render SFSR_Inseason_Escapement.Rmd to a file index.html
#   to post to a GitHub page
#
# Authors: Mike Ackerman
# 
# Created: June 6, 2026
#   Last Modified: June 5, 2026

# clear environment
rm(list = ls())

# load libraries
library(here)
library(rmarkdown)

# render to index.html
rmarkdown::render(
  input = here("docs/SFSR_Inseason_Escapement.Rmd"),
  output_file = "index.html",
  output_dir  = here("docs")
)

### END SCRIPT
