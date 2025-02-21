#' break stress by quantiles
#' 
#' @param world a world object, e.g. the output of \code{\link{gen_world}}.
#' @param breaks a named vector of quantile breaks to apply. The names are used
#'   as labels. E.g. c("A" = 0, "B" = 0.5) would divide the lower half of the
#'   world as "A", and the upper half of the world as "B".
#' 
#' @return an updated world object
#' 
#' @export
#' 
classify_heights <- function(world, breaks) {
  stress <- quantile(world$map$stress, breaks)

  world$map$topography <- NA

  for (i in 1:length(breaks)) {
    these <- world$map$stress >= stress[i]
    world$map$topography[these] <- names(breaks)[i]
  }

  world$map$topography <- factor(world$map$topography,
                               levels = rev(names(breaks)))
  return(world)
}


#' break temperatures by quantiles
#' 
#' @param world a world object, e.g. the output of \code{\link{gen_world}}.
#' @param breaks a named vector of quantile breaks to apply. The names are used
#'   as labels. E.g. c("A" = 0, "B" = 0.5) would divide the colder half of the
#'   world as "A", and the warmer half of the world as "B".
#' 
#' @return an updated world object
#' 
#' @export
#' 
classify_temps <- function(world, breaks) {
  world$map$biome <- NA

  for (i in 1:length(breaks)) {
    these <- world$map$temperature >= breaks[i]
    world$map$biome[these] <- names(breaks)[i]
  }

  world$map$biome <- factor(world$map$biome,
                            levels = rev(names(breaks)))
  return(world)
}
