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
