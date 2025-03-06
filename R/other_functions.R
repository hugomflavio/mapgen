#' Consider NA's as FALSE
#'
#' Aimed to be used in a vector of TRUE/FALSE's, where NA's are present and should be considered as false.
#'
#' @param input vector containing NA's.
#'
#' @return A logical vector.
#'
#' @keywords internal
#'
na_as_false <- function(input) {
  input[is.na(input)] <- FALSE
  return(input)
}

#' Scale stress values
#' 
#' \code{\link{calc_stress}} can generate different ranges of values for
#' different parameters. This function scales stress to roughly -1000 to 1000.
#' 
#' @param world A world list containing stress values. The output of
#'   \code{\link{calc_stress}}.
#' 
#' @return an updated world list where the stress data has been scaled.
#' 
#' @export
#' 
scale_stress <- function(world) {
  world$map$stress <- world$map$stress - mean(world$map$stress)
  world$map$stress <- world$map$stress / sd(world$map$stress)
  world$map$stress <- world$map$stress * 333
  return(world)
}

smoothen <- function(world, spread) {
  pb <- txtProgressBar(min = 0, max = nrow(world$map), style = 3, width = 60)
  counter <- 0
  new_stress <- sapply(1:nrow(world$map), function(i) {
    counter <<- counter + 1
    setTxtProgressBar(pb, counter)
    neighbours <- neighbour_ids(world, i)
    # if all neighbours belong to same plate, skip
    if (length(unique(world$map$plate[neighbours])) == 1) {
      return(0)
    # otherwise...
    } else {
      mean(world$map$stress[neighbours]) - world$map$stress[i]
    }
  })
  close(pb)
  # plates already have a base level. This adds to that
  # base level.
  world$map$stress <- world$map$stress + new_stress
  return(world)
}


tile_loc <- function(world, i) {
  paste0("(", world$map$x[i], ", ", world$map$y[i], ")")
}

tile_id <- function(world, x, y) {
  which(world$map$x == x & world$map$y == y)
}

neighbour_ids <- function(world, i, dist = 1) {
  # determine the range of nearby cells
  range_x <- wrapped_range(world$map$x[i], dist, world$map_x)
  range_y <- wrapped_range(world$map$y[i], dist, world$map_y)

  # pick the cells that match the x and y parameters
  rows_x <- world$map$x %in% range_x
  rows_y <- world$map$y %in% range_y
  return(which(rows_x & rows_y))
}
