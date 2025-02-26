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
gen_plates <- function(n_plates, map_x, map_y = map_x,
                       gravity_range = c(0.2, 1),
                       height_range = 10,
                       dist_method = c("square", "manhattan")) {

  dist_method <- match.arg(dist_method)

  plates <- data.frame(id = 1:n_plates,
                       centre_x = round(runif(n = n_plates, 1, map_x)),
                       centre_y = round(runif(n = n_plates, 1, map_y)),
                       force_x = runif(n = n_plates, -1, 1),
                       force_y = runif(n = n_plates, -1, 1),
                       gravity = runif(n = n_plates,
                                       gravity_range[1],
                                       gravity_range[2])^2,
                       base_height = runif(n = n_plates,
                                           -height_range,
                                           height_range))

  plates$force_scaled_x <- plates$centre_x + plates$force_x * (map_x/5)
  plates$force_scaled_y <- plates$centre_y + plates$force_y * (map_y/5)

  map <- expand.grid(x = 1:map_x, y = 1:map_y)

  map$plate <- apply(map, 1, function(r) {
    x_dists <- abs(wrapped_distance(r["x"], plates$centre_x, map_x))^2 / plates$gravity
    y_dists <- abs(wrapped_distance(r["y"], plates$centre_y, map_y))^2 / plates$gravity
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

  tiles_per_plate <- table(map$plate)
  plates$n_tiles <- 0
  link <- match(plates$id, names(tiles_per_plate))
  plates$n_tiles[link] <- tiles_per_plate[link]

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
gen_world <- function(n_plates, stress, smooth,
                      weight = 1, noise = 50,
                      map_x, map_y = map_x,
                      gravity_range = c(0.2, 1),
                      height_range = 10,
                      dist_method = c("square", "manhattan")) {
  dist_method <- match.arg(dist_method)

  # make the worlds
  recipient <- lapply(1:length(n_plates), function(i) {
    world <- gen_plates(n_plates[i], map_x = map_x, map_y = map_y,
                        gravity_range = gravity_range,
                        height_range = height_range[i],
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


# Main temperature function
gen_temperature <- function(world,
                        pole_locs, pole_radius = 20,
                        pole_power = 2,
                        hotspot_locs = list(c(1, 1)), hotspot_radius = 5,
                        hotspot_power = 0.2,
                        noise = NULL,
                        min_land_effect = 0,
                        max_land_effect = 15,
                        min_water_effect = 3,
                        max_water_effect = 5) {

  map_x <- max(world$map$x)
  map_y <- max(world$map$y)

  if (missing("pole_locs")) {
    pole_locs <- list(c(map_x/2, map_y+0.5))
  }

  pole_radius <- rep(pole_radius, length.out = length(pole_locs))
  hotspot_radius <- rep(hotspot_radius, length.out = length(hotspot_locs))

  pb <- txtProgressBar(min = 0, max = nrow(world$map), style = 3, width = 60)
  counter <- 0
  world$map$temperature_base <- apply(world$map, 1, function(r) {
    counter <<- counter + 1
    setTxtProgressBar(pb, counter)
    # Calculate the distance to the pole
    dist_to_pole <- sapply(pole_locs, function(pole) {
      x_dists <- abs(wrapped_distance(as.numeric(r["x"]), pole[1], map_x))
      y_dists <- abs(wrapped_distance(as.numeric(r["y"]), pole[2], map_y))
      output <- sqrt(x_dists^2 + y_dists^2)
      return(output)
    })
    # Find the minimum distance to the hotspot
    this_pole <- which.min(dist_to_pole)

    # Calculate the temperature based on distance to the pole
    if (dist_to_pole > pole_radius) {
      pole_minus_r <- dist_to_pole[this_pole] - pole_radius[this_pole]
      pole_plus_r <- dist_to_pole[this_pole] + pole_radius[this_pole]
      scaled_temp <- ((pole_minus_r) / (pole_plus_r)) ^ pole_power
    } else {
      scaled_temp <- 0
    }

    # Calculate distances to all hotspot locations
    dist_to_hot <- sapply(hotspot_locs, function(hot) {
      x_dists <- abs(wrapped_distance(as.numeric(r["x"]), hot[1], map_x))
      y_dists <- abs(wrapped_distance(as.numeric(r["y"]), hot[2], map_y))
      output <- sqrt(x_dists^2 + y_dists^2)
      return(output)
    })
    # Find the minimum distance to the hotspot
    this_hot <- which.min(dist_to_hot)

    # Calculate the hotspot modifier
    hot_minus_r <- dist_to_hot[this_hot] - hotspot_radius[this_hot]
    hot_plus_r <- dist_to_hot[this_hot] + hotspot_radius[this_hot]
    hot_modifier <- hot_minus_r / (hot_plus_r * 2)

    # Calculate the noise
    if (is.null(noise)) {
      heat_noise <- 0
    } else {
      heat_noise <- noise$amplitude * ambient::gen_simplex(
        x = as.numeric(r["x"]) * noise$frequency,
        y = as.numeric(r["y"]) * noise$frequency
      )
    }

    # Adjust temperature by the hotspot modifier
    scaled_temp <- scaled_temp - hot_modifier * hotspot_power + heat_noise

    if (scaled_temp < 0) {
      scaled_temp <- 0
    }

    return(scaled_temp)
  })
  close(pb)

  # make temperature ranges more realistic
  max_temp <- max(world$map$temperature_base)
  world$map$temperature_base <- world$map$temperature_base / max_temp
  world$map$temperature_base <- (world$map$temperature_base - 0.5) * 100

  # incorporate terrain effects
  world$map$temperature_real <- world$map$temperature_base

  scaled_stress <- world$map$stress / max(world$map$stress)

  land_effect <- sapply(scaled_stress, function(i) {
    max(min_land_effect, max_land_effect * i)
  })
  water_effect <- sapply(scaled_stress, function(i) {
    max(min_water_effect, max_water_effect * i)
  })
  world$map$temperature_real[world$map$land] <-
    world$map$temperature_real[world$map$land] - land_effect[world$map$land]

  # water has a smaller impact on temperature
  world$map$temperature_real[!world$map$land] <-
    world$map$temperature_real[!world$map$land] - water_effect[!world$map$land]

  return(world)
}

gen_hotspot_locs <- function(world, n = 3) {
  map_x <- max(world$map$x)
  map_y <- max(world$map$y)

  hotspot_x <- rnorm(n, mean = 0, sd = 0.4) * map_x
  hotspot_y <- rnorm(n, mean = 0.5, sd = 0.15) * map_y
  output <- lapply(1:n, function(i) {
    c(hotspot_x[i], hotspot_y[i])
  })

  return(output)
}
