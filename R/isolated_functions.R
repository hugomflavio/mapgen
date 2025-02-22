find_isolated <- function(world) {
  if (is.null(world$map$topography)) {
    stop("This world has no topography yet.", call. = FALSE)
  }

  map_x <- max(world$map$x) 
  map_y <- max(world$map$y)
  pb <- txtProgressBar(min = 0, max = nrow(world$map), style = 3, width = 60)
  counter <- 0
  world$map$isolated <- apply(world$map, 1, function(r) {
    counter <<- counter + 1
    setTxtProgressBar(pb, counter)
    # determine the range of nearby cells
    range_x <- wrapped_range(as.numeric(r["x"]), 1, map_x)
    range_y <- wrapped_range(as.numeric(r["y"]), 1, map_y)

    # pick the cells that match the x and y parameters
    rows_x <- world$map$x %in% range_x
    rows_y <- world$map$y %in% range_y
    neighbours <- world$map[rows_x & rows_y, ]

    # if tile is land
    if (as.logical(r["land"])) {
      if (sum(neighbours$land) == 1) {
        # found one-tile island
        return(TRUE)
      } else {
        return(FALSE)
      }
    # if the tile is water
    } else {
      if (sum(neighbours$land) == 8) {
        # found one-tile lake
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  })
  close(pb)

  single_islands <- world$map$isolated & world$map$land
  single_lakes <- world$map$isolated & !world$map$land
  
  message("Found ", sum(single_islands), " isolated land tile(s).")
  message("Found ", sum(single_lakes), " isolated water tile(s).")
  
  return(world)
}

remove_isolated <- function(world, type = c("both", "land", "water")) {
  type <- match.arg(type)

  if (is.null(world$map$isolated)) {
    stop("Isolated tiles haven't been found yet.", call. = FALSE)
  }

  if (all(!world$map$isolated)) {
    message("This world has no isolated tiles. Doing nothing.")
    return(world)
  }

  if (type == "both") {
    to_fix <- which(world$map$isolated)
  }
  if (type == "land") {
    to_fix <- which(world$map$isolated & world$map$land)
  }
  if (type == "water") {
    to_fix <- which(world$map$isolated & !world$map$land)
  }

  map_x <- max(world$map$x) 
  map_y <- max(world$map$y)
  for (i in to_fix) {
    # determine the range of nearby cells
    range_x <- wrapped_range(as.numeric(world$map$x[i]), 1, map_x)
    range_y <- wrapped_range(as.numeric(world$map$y[i]), 1, map_y)

    # pick the cells that match the x and y parameters
    rows_x <- world$map$x %in% range_x
    rows_y <- world$map$y %in% range_y
    neighbours <- world$map[rows_x & rows_y, ]

    world$map$stress[i] <- mean(neighbours$stress)
    world$map$topography[i] <- names(table(neighbours$topography))[1]
    world$map$land[i] <- as.logical(names(table(neighbours$land))[1])
    world$map$isolated[i] <- FALSE
  }

  message("Removed ", length(to_fix), " isolated tile(s).")

  return(world)
}
