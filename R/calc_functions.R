#' Calculate stress between plates
#' 
#' @param world A world list; the output of \code{\link{gen_plates}}
#' @param spread How many tiles away from the plate border should stress
#'   start being felt? Defaults to 5.
#' 
#' @return an updated world list, where the map contains a stress column
#' 
#' @export
#' 
calc_stress <- function(world, spread = 5) {
  pb <- txtProgressBar(min = 0, max = nrow(world$map), style = 3, width = 60)
  counter <- 0
  new_stress <- sapply(1:nrow(world$map), function(i) {
    counter <<- counter + 1
    setTxtProgressBar(pb, counter)
    neighbours <- neighbour_ids(world, i, dist = spread)
    neigh_vec <- rep(FALSE, nrow(world$map))
    neigh_vec[neighbours] <- TRUE
    # if all neighbours belong to same plate, skip
    if (length(unique(world$map$plate[neighbours])) == 1) {
      return(0)
    # otherwise...
    } else {
      not_r_plate <- neigh_vec & world$map$plate != world$map$plate[i]
  
      rel_x <- wrapped_distance(world$map$x[i],
                                world$map$x[not_r_plate],
                                world$map_x)
      rel_y <- wrapped_distance(world$map$y[i],
                                world$map$y[not_r_plate],
                                world$map_y)

      rel_force_x <- world$map$force_x[not_r_plate] - world$map$force_x[i]
      rel_force_y <- world$map$force_y[not_r_plate] - world$map$force_y[i]
      force <- sqrt(rel_force_x^2 + rel_force_y^2)
      stress <- (rel_x * rel_force_x / force) + (rel_y * rel_force_y / force)
      return(-sum(stress))
    }
  })
  close(pb)
  # plates already have a base level. This adds to that
  # base level.
  world$map$stress <- world$map$stress + new_stress
  return(world)
}

calc_slope <- function(world, these, show_progress = TRUE) {
  if (missing(these)) {
    these <- 1:nrow(world$map)
  }
  if (show_progress) {
    pb <- txtProgressBar(min = 0, max = length(these), style = 3, width = 60)
  }
  counter <- 0
  recipient <- lapply(these, function(i) {
    counter <<- counter + 1
    if (show_progress) {
      setTxtProgressBar(pb, counter)
    }

    # determine the range of nearby cells
    link <- neighbour_ids(world, i)
    neighbours <- world$map[link, ]
    rownames(neighbours) <- 1:9
    
    range_x <- wrapped_range(world$map$x[i], 1, world$map_x)
    link <- match(neighbours$x, range_x)
    names(link) <- row.names(neighbours)
    neighbours <- neighbours[names(sort(link)), ]

    range_y <- wrapped_range(world$map$y[i], 1, world$map_y)
    link <- match(neighbours$y, range_y)
    names(link) <- row.names(neighbours)
    neighbours <- neighbours[names(sort(link)), ]
    rownames(neighbours) <- 1:9
    # row order compared to tile structure:
    # 7 8 9 
    # 4 5 6
    # 1 2 3
    slope_dir <- which(neighbours$stress == min(neighbours$stress))
    # if two or more neighbouring tiles have exactly the same stress
    if (length(slope_dir) > 1) {
      # if any of them is towards itself, pick that one
      if (any(slope_dir == 5)) {
        slope_dir <- 5
      }
      # in the up/down/left/right direction, pick that one
      if (any(slope_dir %in% c(2, 4, 6, 8))) {
        slope_dir <- slope_dir[slope_dir %in% c(2, 4, 6, 8)][1]
      # else just pick the first one
      } else {
        slope_dir <- slope_dir[1]
      }
    }
    output <- data.frame(
      slope = max(neighbours$stress) - min(neighbours$stress),
      slope_dir = slope_dir)
    return(output)
  })
  if (show_progress) {
    close(pb)
  }
  slopes <- do.call(rbind, recipient)
  world$map$slope[these] <- slopes$slope
  world$map$slope_dir[these] <- slopes$slope_dir
  return(world)
}
