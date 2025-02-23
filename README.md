# mapgen

# Installation instructions

## Installing R

If you don't have install R, find the right version for you [in the CRAN website](https://cloud.r-project.org/) and install it.

Most people these days use [RStudio](https://posit.co/download/rstudio-desktop/), but it is not essential to run R. If you plan on working from the terminal, you don't need it.


## Installing development tools (you don't need this if you're not developing the package)

To easily document, check, and load package functions while working on the code, you'll want to have the package devtools installed. I would recommend opening a sudo (admin) session of R to install packages. Then, run:

```r
install.packages("devtools")
```

## Installing mapgen

You don't need this if you're developing the package directly from a local repository (path above). To install mapgen from github, you'll need to run two lines of code. I would recommend opening a sudo (admin) session of R to install packages. Then, run:

```r
install.packages("devtools")
remotes::install_github("hugomflavio/mapgen")
```

## developing mapgen

To work on developing mapgen, you should make a local copy of the repository. Then, move your R session to the root of the repository.

Most of the active development is made in the "working_script.R". At the top of that script, you'll see the command `devtools::load_all()`. This loads all the functions currently defined in the `R/` folder. You can edit those functions, re-run `devtools::load_all()`, and run the example you're working on again, to test out the new changes.

