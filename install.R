# .Rprofile file for project-specific settings
required_packages <- c(
  "devtools",
  "ggplot2",
  "patchwork",
  "gridExtra",
  "reshape2"
)

# Emsure project library is created
dir.create(project_lib_dir, showWarnings = FALSE, recursive = TRUE)

# Install any missing packages
library("utils")
missing <- !(required_packages %in% rownames(installed.packages()))
install.packages(required_packages[missing], lib = project_lib_dir)

# Verify packages load
lapply(required_packages, library, character.only = TRUE)

