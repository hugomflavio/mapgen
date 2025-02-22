# .Rprofile file for project-specific settings
project_lib_dir <- "./libs"
default_packages <- c(
  "devtools",
  "patchwork"
)

# Set up project library
.libPaths(c(project_lib_dir, .libPaths()))

# Load default packages
library("utils")
installed <- default_packages %in% rownames(installed.packages())
lapply(default_packages[installed], library, character.only = TRUE)

