assign_bodies <- function(world) {
  pb <- txtProgressBar(min = 0, max = nrow(world$map), style = 3, width = 60)
  land_counter <- 0
  water_counter <- 0
  world$map$body <- NA
  for (i in 1:nrow(world$map)) {
  # for (i in 1) {
    setTxtProgressBar(pb, i)

    link <- neighbour_ids(world, i)
    neighbours <- world$map[link, ]
    # look only at cells that are of the same land type
    neighbours <- neighbours[neighbours$land == world$map$land[i], ]
    # if any already has a number, we're in the same
    if (any(!is.na(neighbours$body))) {
      touched <- unique(neighbours$body)
      touched <- touched[!is.na(touched)]
      if (world$map$land[i]) {
        keep <- min(touched)
      } else {
        keep <- max(touched)
      }
      if (length(touched) > 1) {
        for (j in touched[touched != keep]) {
          link <- na_as_false(world$map$body == j)
          world$map$body[link] <- keep
        }
      }
      world$map$body[i] <- keep
    # otherwise, assign a new number
    } else {
      if (world$map$land[i]) {
        land_counter <- land_counter + 1
        world$map$body[i] <- land_counter
      } else {
        water_counter <- water_counter - 1
        world$map$body[i] <- water_counter
      }
    }
  }
  close(pb)

  bodies <- aggregate(world$map$body,
                      list(body = world$map$body),
                      length)
  colnames(bodies)[2] <- "n_tiles"
  bodies$x <- aggregate(world$map$x,
                        list(body = world$map$body),
                        median)$x
  bodies$y <- aggregate(world$map$y,
                        list(body = world$map$body),
                        median)$x
  bodies$land <- aggregate(world$map$land,
                        list(body = world$map$body),
                        all)$x

  world$bodies <- bodies

  return(world)
}

assign_vertical_terrain <- function(world) {
  if (is.null(world$topography)) {
    stop("No topography information found. Aborting.", call. = FALSE)
  }

  if (!is.null(world$map$terrain)) {
    warning("terrain already exists. deleting old terrain.", call. = FALSE)
  }
  world$map$terrain <- NA

  for (j in c("pct_hills", "pct_mountains")) {
    if (!is.null(world$topography[, j])) {
      for (i in 1:nrow(world$topography)) {
        if (world$topography[i, j] > 0) {
          link <- world$map$topography == world$topography$id[i]
          if (any(link)) {
            # redundant here, but keeping. All tiles should be NA at this point
            link_available <- link & is.na(world$map$terrain)
            n_tiles <- min(sum(link_available),
                           ceiling(sum(link) * world$topography[i, j] / 100))
            index <- sample(which(link_available), n_tiles, replace = FALSE)
            world$map$terrain[index] <- sub("pct_", "", j)
          }
        }
      }
    }
  }

  return(world)
}

assign_water_terrain <- function(world, lake_threshold = 50) {
  world$map$ocean <- FALSE
  world$map$terrain[world$map$topography == "deep_water"] <- "deep ocean"
  world$map$ocean[world$map$topography == "deep_water"] <- TRUE
  world$map$terrain[world$map$topography == "water"] <- "ocean"
  world$map$ocean[world$map$topography == "water"] <- TRUE
  small <- world$bodies$n_tiles <= lake_threshold
  waterbodies <- !(world$bodies$land)
  lakes <- world$bodies$body[small & waterbodies]
  world$map$terrain[world$map$body %in% lakes] <- "lake"
  world$map$ocean[world$map$body %in% lakes] <- FALSE
  return(world)
}
