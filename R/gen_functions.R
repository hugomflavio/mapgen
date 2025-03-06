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

  map <- expand.grid(x = 1:map_x,
                     y = 1:map_y)
  map$id <- id <- 1:nrow(map)

  map$plate <- sapply(1:nrow(map), function(i) {
    x_dists <- abs(wrapped_distance(map$x[i], plates$centre_x, map_x))^2 / plates$gravity
    y_dists <- abs(wrapped_distance(map$y[i], plates$centre_y, map_y))^2 / plates$gravity
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

  output <- list(plates = plates, vectors = vectors, 
                 map = map, map_x = map_x, map_y = map_y)
  return(output)
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
    message("M: generating plates for layer ", i)
    world <- gen_plates(n_plates[i], map_x = map_x, map_y = map_y,
                        gravity_range = gravity_range,
                        height_range = height_range[i],
                        dist_method = dist_method)
    if (smooth[i] != 0) {
      message("M: smoothening plates for layer ", i)
      world <- smoothen(world, spread = smooth[i])
    }
    if (stress[i] != 0) {
      message("M: calculating stress for layer ", i)
      world <- calc_stress(world, spread = stress[i])
      world <- scale_stress(world)
    }
    return(world)
  })

  # use the first world as the reference
  final_world <- recipient[[1]]

  # merge any other worlds to the first one
  if (length(n_plates) > 1) {
    message("M: Merging layers")
    for (i in 2:length(n_plates)) {
      final_world$map$stress <- 
        final_world$map$stress + recipient[[i]]$map$stress * weight
    }
  }

  final_world <- scale_stress(final_world)

  if (noise > 0) {
    message("M: Adding noise")
    aux <- rnorm(nrow(final_world$map), 0, noise)
    final_world$map$stress <- final_world$map$stress + aux
  }

  message("M: Done")
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

  if (is.null(noise) || is.null(noise$seed)) {
    seed <- sample.int((2^31 - 1), 1)
  } else {
    seed <- noise$seed
  }

  if (missing("pole_locs")) {
    pole_locs <- list(c(world$map_x/2, world$map_y+0.5))
  }

  pole_radius <- rep(pole_radius, length.out = length(pole_locs))
  hotspot_radius <- rep(hotspot_radius, length.out = length(hotspot_locs))

  pb <- txtProgressBar(min = 0, max = nrow(world$map), style = 3, width = 60)
  counter <- 0
  world$map$temperature_base <- sapply(1:nrow(world$map), function(i) {
    counter <<- counter + 1
    setTxtProgressBar(pb, counter)
    # Calculate the distance to the pole
    dist_to_pole <- sapply(pole_locs, function(pole) {
      x_dists <- abs(wrapped_distance(world$map$x[i], pole[1], world$map_x))
      y_dists <- abs(wrapped_distance(world$map$y[i], pole[2], world$map_y))
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
      x_dists <- abs(wrapped_distance(world$map$x[i], hot[1], world$map_x))
      y_dists <- abs(wrapped_distance(world$map$y[i], hot[2], world$map_y))
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
        x = world$map$x[i] * noise$frequency,
        y = world$map$y[i] * noise$frequency,
        seed = seed
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
  hotspot_x <- rnorm(n, mean = 0, sd = 0.4) * world$map_x
  hotspot_y <- rnorm(n, mean = 0.5, sd = 0.15) * world$map_y
  output <- lapply(1:n, function(i) {
    c(hotspot_x[i], hotspot_y[i])
  })

  return(output)
}

gen_springs <- function(world, basin_size = 50,
                        min_island = ceiling(basin_size/2)) {
  candidates <- world$bodies$land & world$bodies$n_tiles >= min_island
  world$bodies$springs <- 0
  world$bodies$springs[candidates] <- 
    ceiling(world$bodies$n_tiles[candidates] / basin_size)

  world$map$spring <- FALSE

  for (i in which(candidates)) {
    on_body <- world$map$body == world$bodies$body[i]
    not_coastal <- !world$map$coastal
    tiles <- which(on_body & not_coastal)

    if (length(tiles) > 1) {
      these <- NULL
      for (j in 1:world$bodies$springs[i]) {
        these[j] <- sample(tiles, 1,
                           prob = world$map$height[tiles])
        # remove tiles adjacent to new spring from the pool
        x_out <- world$map$x[tiles] %in% (world$map$x[these[j]] + (-1:1))
        y_out <- world$map$y[tiles] %in% (world$map$y[these[j]] + (-1:1))
        tiles <- tiles[!(x_out & y_out)]
        if (length(tiles) < 1) {
          # reduce number of springs if not enough space
          world$bodies$springs[i] <- length(these)
          break()
        }
      }
      world$map$spring[these] <- TRUE
    }
  }
  return(world)
}

gen_rivers <- function(world, river_angles = 4) {
  
  if (!(river_angles %in% c(4, 8))) {
    stop("river_angles must be 4 or 8")
  }

  world$map$river <- FALSE
  world$map$river_id <- NA
  if (!getOption("mapgen.debug", default = FALSE)) {
    pb <- txtProgressBar(min = 0, max = sum(world$map$spring),
                         style = 3, width = 60)
  }
  counter <- 0
  for (i in which(world$map$spring)) {
    counter <- counter + 1
    if (!getOption("mapgen.debug", default = FALSE)) {
      setTxtProgressBar(pb, counter)
    }
    keep_flowing <- TRUE
    from <- i
    world$map$river[from] <- TRUE
    world$map$river_id[from] <- counter
    while (keep_flowing) {
      # if (counter %in% 60) {
      #   options(mapgen.debug = TRUE)
      # } else {
      #   options(mapgen.debug = FALSE)        
      # }
      if (getOption("mapgen.debug", default = FALSE)) {
        cat("River", counter, "from", tile_loc(world, from))
        p <- plot_ids(world, world$map$id[na_as_false(world$map$river_id == counter)],
                      label = round(height, 1))
        p <- p + ggplot2::geom_tile(data  = world$map[from, , drop = FALSE],
                                    linewidth = 1.5,
                                    fill = "purple",
                                    col = "purple",
                                    alpha = 0.2,
                                    width = 1, height = 1)
        print(p)
      }
      # # check if we've touched another river.
      near <- neighbour_ids(world, from)
      names(near) <- 1:9
      near <- near[na_as_false(world$map$river_id[near] != counter)]
      if (length(near) > 0) {
        if (river_angles == 4 && !any(names(near) %in% c(2, 4, 6, 8))) {
          # if this happens, then the neighbouring river is in a diagonal.
          # since that is the case, the two rivers can't merge, so we just
          # keep going. as normal.
        } else {
          # if rivers_angles == 8 or the neighbour river tile is in a valid angle
          world <- merge_rivers(world, counter, from, near[1])
          keep_flowing <- FALSE
          # stop here, the river we merged to already reaches the coast
          next
        }
      }
      # if we have landed on a lake that has no river
      # associated to it, we must assign all lake tiles
      # to the river. Then we'll need to start filling it
      # up until it spills over.
      if (!world$map$land[from]) {
        if (getOption("mapgen.debug", default = FALSE)) {
          cat(" - this tile is lake", world$map$body[from], "! Flooding...")
          useless_var <- readline("press enter to continue.")
        }
        lake_id <- world$map$body[from]
        world$map$river[world$map$body == lake_id] <- TRUE
        world$map$river_id[world$map$body == lake_id] <- counter
        aux <- flood_lake(world = world,
                          lake_id = lake_id,
                          river_id = counter,
                          river_angles = river_angles)
        world <- aux$world
        to <- aux$to
        if (getOption("mapgen.debug", default = FALSE)) {
          p <- plot_ids(world, world$map$id[na_as_false(world$map$river_id == counter)],
                        label = round(height, 1))
          p <- p + ggplot2::geom_tile(data  = world$map[from, , drop = FALSE],
                                      linewidth = 1.5,
                                      fill = "purple",
                                      col = "purple",
                                      alpha = 0.2,
                                      width = 1, height = 1)
          print(p)
        }
      } else {
        if (river_angles == 4) {
          to <- .gen_river4(world, from)
        } else {
          to <- .gen_river8(world, from)
        }
      }
      if (getOption("mapgen.debug", default = FALSE)) {
        p <- p + ggplot2::geom_tile(data  = world$map[to, , drop = FALSE],
                                    linewidth = 1.5,
                                    fill = "yellow",
                                    col = "yellow",
                                    alpha = 0.2,
                                    width = 1, height = 1)
        print(p)
      }
      if (to == from) {
        # we're trapped in a basin. Time to flood it.
        world <- fill_basin(world, to)
        next
      }
      if (getOption("mapgen.debug", default = FALSE)) {
        cat(" to", tile_loc(world, to), "\n")
        useless_var <- readline("press enter to continue.")
      }
      # if there's already a river at the next tile,
      # change river id so it is all the same river,
      # and stop flowing this spring
      if (world$map$river[to]) {
        if (world$map$river_id[from] == world$map$river_id[to]) {
          if (!world$map$land[to]) {
            # found the same lake while flooding. 
            # jump to next iteration to keep flooding.
            from <- to
            next
          } else {
            stop("Circular river! No method implemented yet to resolve this.")
          }
        } else {
          world <- merge_rivers(world, counter, from, to)
          keep_flowing <- FALSE
          # stop here, the river we merged to already reaches the coast
          next
        }
      } else {
        world$map$river[to] <- TRUE
        world$map$river_id[to] <- counter
      }
      # if we've reached a coast, wrap it up.
      if (world$map$coastal[to]) {
        world <- finish_river(world, counter, to)
        keep_flowing <- FALSE
      } else {
        # start from new position if not done
        from <- to
      }
    }
  }
  if (!getOption("mapgen.debug", default = FALSE)) {
    close(pb)
  }

  # re-index rivers continuously.
  aux <- world$map$river_id[world$map$river]
  world$map$river_id[world$map$river] <- match(aux, sort(unique(aux)))
  return(world)
}

fill_basin <- function(world, to) {
  if (getOption("mapgen.debug", default = FALSE)) {
    cat(" - this tile is a dry basin! Making lake and trying again.\n")
    useless_var <- readline("press enter to continue.")
  }
  # find if there's an already filling lake nearby
  link <- neighbour_ids(world, to)
  add_to_other <- any(!world$map$land[link])
  # apply the flooding
  if (add_to_other) {
    water_link <- link[!world$map$land[link]]
    world$map$body[to] <- world$map$body[water_link][1]
  } else {
    world$map$body[to] <- min(world$bodies$body) - 1
    new_body <- data.frame(body = min(world$bodies$body) - 1,
                           n_tiles = 1,
                           x = world$map$x[to],
                           y = world$map$y[to],
                           land = FALSE,
                           springs = 0)
    world$bodies <- rbind(new_body, world$bodies)
  }
  world$map$topography[to] <- "water"
  world$map$land[to] <- FALSE
  world$map$terrain[to] <- "lake"
  return(world)
}

merge_rivers <- function(world, counter, from, to) {
  if (getOption("mapgen.debug", default = FALSE)) {
    cat("River", counter, "reached river", world$map$river_id[to],
        "at", tile_loc(world, from), "- merging rivers.\n")
    p_link <- na_as_false(world$map$river_id == world$map$river_id[to])
    p <- p + ggplot2::geom_tile(data  = world$map[p_link, ],
                                linewidth = 1.5,
                                fill = "blue",
        ,                        col = "blue",
                                alpha = 0.2,
                                width = 1, height = 1)
    print(p)
    useless_var <- readline("press enter to continue.")
  }
  link <- na_as_false(world$map$river_id == world$map$river_id[from])
  world$map$river_id[link] <- world$map$river_id[to]
  return(world)
}

find_shores <- function(world, id) {
  if (world$bodies$land[world$bodies$body == id]) {
    output <- lapply(which(world$map$body == id),
                     function(i) {
      link <- neighbour_ids(world, i)
      neighbours <- world$map[link, ]
      if (any(!neighbours$land)) {
        return(i)
      } else {
        return(NULL)
      }
    })
  } else {
    output <- lapply(which(world$map$body == id),
                     function(i) {
      link <- neighbour_ids(world, i)
      neighbours <- world$map[link, ]
      shores <- neighbours[neighbours$land, ]
      return(as.numeric(row.names(shores)))
    })
  }
  output <- do.call(c, output)
  return(sort(unique(output)))
}

flood_lake <- function(world, river_id, lake_id, river_angles) {
  keep_looking <- TRUE
  while(keep_looking) {
    lake_tiles <- which(world$map$body == lake_id)
    all_shores <- find_shores(world, lake_id)
    # recalculate slope shores in case we've already been flooding things.
    world <- calc_slope(world, these = c(lake_tiles, all_shores),
                        show_progress = FALSE)
    if (getOption("mapgen.debug", default = FALSE)) {
      p <- plot_ids(world, world$map$id[na_as_false(world$map$river_id == counter)],
                    label = round(height, 1))
      p <- p + ggplot2::geom_tile(data  = world$map[from, , drop = FALSE],
                                  linewidth = 1.5,
                                  fill = "purple",
                                  col = "purple",
                                  alpha = 0.2,
                                  width = 1, height = 1)
      p <- p + ggplot2::geom_tile(data  = world$map[na_as_false(world$map$id %in% lake_tiles), ],
                                  linewidth = 1.5,
                                  fill = "blue",
                                  col = "blue",
                                  alpha = 0.2,
                                  width = 1, height = 1)
      p <- p + ggplot2::geom_tile(data  = world$map[na_as_false(world$map$id %in% all_shores), ],
                                  linewidth = 1.5,
                                  fill = "brown",
                                  col = "brown",
                                  alpha = 0.2,
                                  width = 1, height = 1)
      print(p)
      useless_var <- readline("press enter to continue.")
    }
    test_shore <- all_shores[which.min(world$map$height[all_shores])]
    # if coastal, we need to make sure it falls within the
    # allowed river directions. for 8, that's everyone.
    if (world$map$coastal[test_shore]) {
      if (getOption("mapgen.debug", default = FALSE)) {
        cat("Test shore is coastal tile.\n")
        useless_var <- readline("press enter to continue.")
      }
      if (river_angles == 4) {
        # find the neighbours of the test_shore
        near <- neighbour_ids(world, test_shore)
        names(near) <- 1:9
        near <- near[!near %in% test_shore]
        # and determine which of those belong to the lake
        near_lake <- near[near %in% lake_tiles]
        # check if that direction is allowed
        if (!any(names(near_lake) %in% c(2, 4, 6, 8))) {
          if (getOption("mapgen.debug", default = FALSE)) {
            cat("Coastal test shore conflicts with river directions. resampling.\n")
            useless_var <- readline("press enter to continue.")
          }
          # if not, find the other nearby shores, and pick
          # the lowest one.
          near_land <- near[world$map$land[near]]
          near_shores <- near_land[near_land %in% all_shores]
          test_shore <- near_shores[which.min(world$map$height[near_shores])] 
        }
      }
    }
    if (getOption("mapgen.debug", default = FALSE)) {
      p <- p + ggplot2::geom_tile(data  = world$map[all_shores, , drop = FALSE],
                                  linewidth = 1.5,
                                  fill = "orange",
                                  col = "orange",
                                  alpha = 0.2,
                                  width = 1, height = 1)
      p <- p + ggplot2::geom_tile(data  = world$map[test_shore, , drop = FALSE],
                                  linewidth = 1.5,
                                  fill = "yellow",
                                  col = "yellow",
                                  alpha = 0.2,
                                  width = 1, height = 1)
      print(p)
      useless_var <- readline("press enter to continue.")
    }
    to <- next_in_dir(world = world,
                      id = test_shore,
                      dir = world$map$slope_dir[test_shore])

    if (!to %in% lake_tiles) {
      if (getOption("mapgen.debug", default = FALSE)) {
        cat("Found a way out...")
        useless_var <- readline("press enter to continue.")
      }
      if (river_angles == 4) {   
        # check that the outflow direction works to start a river                                                                                  
        # otherwise, the new exit is not good for a 4-direction river.
        if (world$map$slope_dir[test_shore] %in% c(2, 4, 6, 8)) {
          keep_looking <- FALSE
          if (getOption("mapgen.debug", default = FALSE)) {
            cat(" way out is valid. Flooding done.\n")
            useless_var <- readline("press enter to continue.")
          }
        } else {
          if (getOption("mapgen.debug", default = FALSE)) {
            cat(" way out conflicts with river dir.\n")
            useless_var <- readline("press enter to continue.")
          }
        }
      # all exits work for 8-direction rivers
      } else {
        if (getOption("mapgen.debug", default = FALSE)) {
          cat(" Flooding done.\n")
          useless_var <- readline("press enter to continue.")
        }
        keep_looking <- FALSE
      }
    }

    if (world$map$coastal[test_shore]) {
      # we don't flood shores, so we need to pull the pointer
      # one up and finish.
      to <- test_shore
      keep_looking <- FALSE
    } else {
      # apply the flooding
      world$map$topography[test_shore] <- "water"
      world$map$land[test_shore] <- FALSE
      world$map$terrain[test_shore] <- "lake"
      world$map$river[test_shore] <- TRUE
      world$map$river_id[test_shore] <- world$map$river_id[world$map$body == lake_id][1]
      world$map$body[test_shore] <- lake_id
      # raise all lake tiles to this level
      world$map$stress[world$map$body == lake_id] <- world$map$stress[test_shore]
      world$map$height[world$map$body == lake_id] <- world$map$height[test_shore]

      # if we've reached another water body, connect them
      near <- neighbour_ids(world, test_shore)
      near <- near[!near %in% c(test_shore, lake_tiles)]
      if (any(!world$map$land[near])) {
        if (getOption("mapgen.debug", default = FALSE)) {
          cat("We've reached another water body.\n")
          useless_var <- readline("press enter to continue.")
        }
        near_id <- near[!world$map$land[near]]
        near_body <- unique(world$map$body[near_id])
        if (length(near_body) > 1) {
          stop("Touched two water bodies at the same time!")
        }
        # raise all lake tiles to highest level
        the_stress <- max(world$map$stress[world$map$body == lake_id],
                          world$map$stress[world$map$body == near_body])
        the_height <- max(world$map$height[world$map$body == lake_id],
                          world$map$height[world$map$body == near_body])
        world$map$stress[world$map$body == lake_id] <- the_stress
        world$map$stress[world$map$body == near_body] <- the_stress
        world$map$height[world$map$body == lake_id] <- the_height
        world$map$height[world$map$body == near_body] <- the_height
        # if lake already belongs to a river, stop looking
        if (world$map$river[world$map$body == near_body][1]) {
          if (getOption("mapgen.debug", default = FALSE)) {
            cat("Other water body already belongs to a river. Flooding done.\n")
            useless_var <- readline("press enter to continue.")
          }
          keep_looking <- FALSE
        } else {
          if (getOption("mapgen.debug", default = FALSE)) {
            cat("Other water body hadn't been claimed yet. continuing flooding.\n")
            useless_var <- readline("press enter to continue.")
          }
          world$map$river[world$map$body == near_body][1] <- TRUE
          world$map$river_id[world$map$body == near_body][1] <- river_id
        }
        # harmonize the IDs
        world$map$body[world$map$body == near_body] <- lake_id
      }
    }
  }
  # return the updated world and the way out so gen_river can keep going
  output <- list(world = world, to = to)
  return(output)
}

next_in_dir <- function(world, id, dir) {
  range_x <- wrapped_range(world$map$x[id], 1, world$map_x)
  range_y <- wrapped_range(world$map$y[id], 1, world$map_y)
  # 3 | 7 8 9 
  # 2 | 4 5 6
  # 1 | 1 2 3
  # Y   -----
  #   X 1 2 3
  to <- switch(as.character(dir),
    "1" = which(world$map$x == range_x[1] & world$map$y == range_y[1]),
    "2" = which(world$map$x == range_x[2] & world$map$y == range_y[1]),
    "3" = which(world$map$x == range_x[3] & world$map$y == range_y[1]),
    "4" = which(world$map$x == range_x[1] & world$map$y == range_y[2]),
    "5" = which(world$map$x == range_x[2] & world$map$y == range_y[2]),
    "6" = which(world$map$x == range_x[3] & world$map$y == range_y[2]),
    "7" = which(world$map$x == range_x[1] & world$map$y == range_y[3]),
    "8" = which(world$map$x == range_x[2] & world$map$y == range_y[3]),
    "9" = which(world$map$x == range_x[3] & world$map$y == range_y[3]))
  return(to)
}

.gen_river8 <- function(world, from) {
  to <- next_in_dir(world, from, world$map$slope_dir[from])
  return(to)
}

.gen_river4 <- function(world, from) {
  dir <- world$map$slope_dir[from]
  # 3 |   8 
  # 2 | 4 5 6
  # 1 |   2  
  # Y   -----
  #   X 1 2 3
  dir <- switch(as.character(dir),
                "1" = c(2, 4),
                "2" = 2,
                "3" = c(2, 6),
                "4" = 4,
                "5" = 5,
                "6" = 6,
                "7" = c(4, 8),
                "8" = 8,
                "9" = c(6, 8))

  options <- sapply(dir, function(x) next_in_dir(world, from, x))
  to <- options[which.min(world$map$height[options])]
  return(to)
}

finish_river <- function(world, counter, to) {
  if (getOption("mapgen.debug", default = FALSE)) {
    cat("River", counter, "reached the coast. Wrapping up.\n")
    useless_var <- readline("press enter to continue.")
  }
  if (river_angles == 4) {
    # check there's water in a valid direction
    near <- neighbour_ids(world, to)
    names(near) <- 1:9
    near <- near[!near %in% world$map$id[world$map$river_id == counter]]
    outs <- names(near)[!world$map$land[near] & world$map$ocean[near]]
    good_outs <- outs[outs %in% c(2, 4, 6, 8)]
    if (length(good_outs) == 0) {
      if (getOption("mapgen.debug", default = FALSE)) {
        cat("Ocean is diagonal to coast. Ending at next-best shore.\n")
        useless_var <- readline("press enter to continue.")
      }
      # if the ocean is diagonal, find another tile
      # to hop to before finishing
      outs <- switch(outs[1],
                    "1" = c(2, 4),
                    "3" = c(2, 6),
                    "7" = c(4, 8),
                    "9" = c(6, 8))
      options <- sapply(outs, function(x) next_in_dir(world, to, x))
      new_to <- options[which.min(world$map$height[options])]
      world$map$river[new_to] <- TRUE
      world$map$river_id[new_to] <- counter
      # check that the new_to doesn't open to unclaimed lakes
      lake_outs <- names(near)[!world$map$land[near] & !world$map$ocean[near]]
      if (length(lake_outs) > 0) {
        near_lakes <- unique(world$map$body[near[lake_outs]])
        # in case there's more than one lake. I don't think this can
        # happen, but doesn't hurt to wrap in a for-loop
        for (this_lake in near_lakes) {
          claimed <- world$map$river[world$map$body == this_lake][1]
          if (!claimed) {
            if (getOption("mapgen.debug", default = FALSE)) {
              cat("Found an unclaimed lake next to the shore. claiming it.\n")
              useless_var <- readline("press enter to continue.")
            }
            world$map$river[world$map$body == this_lake] <- TRUE
            world$map$river_id[world$map$body == this_lake] <- counter
          }
        }
      } 
    }
  }
  if (getOption("mapgen.debug", default = FALSE)) {
    cat("Plotting final river system.\n")
    p <- plot_ids(world, world$map$id[na_as_false(world$map$river_id == counter)],
                label = round(height, 1))
    print(p)
    useless_var <- readline("press enter to continue.")
  }
  return(world)
}
