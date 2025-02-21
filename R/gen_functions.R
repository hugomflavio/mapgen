#' Generate tectonic plates
#' 
#' @param n_plates number of plates on the map
#' @param map_x map x dimension in tiles
#' @param map_y map y dimension in tiles
#' 
#' @return a list containing plate information, vector information, and the map
#' 
#' @export
#' 
gen_plates <- function(n_plates, map_x, map_y = map_x, gravity_range = c(1, 1),
                       dist_method = c("square", "manhattan")) {

  dist_method <- match.arg(dist_method)

  plates <- data.frame(id = 1:n_plates,
                       centre_x = round(runif(n = n_plates, 1, map_x)),
                       centre_y = round(runif(n = n_plates, 1, map_y)),
                       force_x = runif(n = n_plates, -1, 1),
                       force_y = runif(n = n_plates, -1, 1),
                       gravity = runif(n = n_plates, gravity_range[1],
                                       gravity_range[2]),
                       base_height = runif(n = n_plates, 1, 100))

  plates$force_scaled_x <- plates$centre_x + plates$force_x * (map_x/5)
  plates$force_scaled_y <- plates$centre_y + plates$force_y * (map_y/5)

  map <- expand.grid(x = 1:map_x, y = 1:map_y)

  map$plate <- apply(map, 1, function(r) {
    x_dists <- abs(wrapped_distance(r["x"], plates$centre_x, map_x)) / plates$gravity
    y_dists <- abs(wrapped_distance(r["y"], plates$centre_y, map_y)) / plates$gravity
    if (dist_method == "manhattan") {
      output <- which.min(x_dists + y_dists)
    } else {
      output <- which.min(sqrt(x_dists^2 + y_dists^2))
    }
    return(output)
  })

  map$force_x <- plates$force_x[map$plate]
  map$force_y <- plates$force_y[map$plate]
  map$centre_x <- plates$centre_x[map$plate]
  map$centre_y <- plates$centre_y[map$plate]
  map$stress <- plates$base_height[map$plate]

  part1 <- plates[, c("id", "centre_x", "centre_y")]
  part2 <- plates[, c("id", "force_scaled_x", "force_scaled_y")]
  colnames(part1) <- colnames(part2) <- c("id", "x", "y")
  vectors <- rbind(part1, part2)

  return(list(plates = plates, vectors = vectors, map = map))
}

#' wrapper to generate a world
#' 
#' Uses one or more sets of plates to create a complex surface.
#' 
#' @param n_plates The number of plates in the map. Can be either 1 value
#'   for a world generated from a single map, or a vector of values for a world
#'   generated of several tectonic maps superimposed.
#' @param spread How many tiles away from the plate border should stress
#'   start being felt? Must be a vector of the same length as n_plates
#' @param weight when more than one map is generated, how strongly should
#'   subsequent maps imprint on the first? defaults to 1; i.e. sub-maps fully
#'   imprint on first map.
#' @param noise Strength of the noise to add to the heights at the end of the
#'   process. noise is the standard deviation of the normal distribution from
#'   which the values are drawn. Set to 0 to not add noise.
#' @inheritParams gen_plates
#' 
#' @return A world object.
#' 
#' @export
#' 
gen_world <- function(n_plates, spread, weight = 1, noise = 50,
                      map_x, map_y = map_x, gravity_range = c(1, 1),
                      dist_method = c("square", "manhattan")) {

  dist_method <- match.arg(dist_method)

  # make the worlds
  recipient <- lapply(1:length(n_plates), function(i) {
    world <- gen_plates(n_plates[i], map_x = map_x, map_y = map_y,
                        gravity_range = gravity_range,
                        dist_method = dist_method)
    world <- calc_stress(world, spread = spread[i])
    world <- scale_stress(world)
  })

  # use the first world as the reference
  final_world <- recipient[[1]]

  # merge any other worlds to the first one
  if (length(n_plates) > 1) {
    for (i in 2:length(n_plates)) {
      final_world$map$stress <- 
        final_world$map$stress + recipient[[i]]$map$stress * weight
    }
  }

  final_world <- scale_stress(final_world)

  if (noise > 0) {
    aux <- rnorm(nrow(final_world$map), 0, noise)
    final_world$map$stress <- final_world$map$stress + aux
  }

  return(final_world)
}
