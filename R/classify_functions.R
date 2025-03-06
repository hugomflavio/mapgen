#' break stress by quantiles
#' 
#' @param world a world object, e.g. the output of \code{\link{gen_world}}.
#' @param topography a topography table containing at least an id column and a
#'   q column (with the minimum quantile for each terrain id).
#' 
#' @return an updated world object
#' 
#' @export
#' 
classify_heights <- function(world, topography) {

  world$topography <- topography

  stress <- quantile(world$map$stress, topography$q)

  world$map$topography <- NA

  for (i in 1:nrow(topography)) {
    these <- world$map$stress >= stress[i]
    world$map$topography[these] <- topography$id[i]
  }

  world$map$topography <- factor(world$map$topography,
                               levels = topography$id)

  world$map$land <- TRUE
  world$map$land[grepl("water", world$map$topography)] <- FALSE

  world$map$height <- world$map$stress
  world$map$height <- world$map$height - min(world$map$height[world$map$land])
  world$map$height[world$map$height < 0] <- 0

  report <- as.data.frame(table(world$map$topography))
  colnames(report) <- c("type", "n_tiles")
  report$pct <- round(report$n_tiles / sum(report$n_tiles) * 100, 2)

  world$topography <- cbind(world$topography, report[, -1])

  # printing
  report <- capture.output(print(report, topn = nrow(report)))
  report <- sub("^\\d", " ", report)
  report <- paste0(report, collapse = "\n")
  message("Tile totals:\n", report)

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
  world$map$temperature_zone <- NA

  for (i in 1:length(breaks)) {
    these <- world$map$temperature_real >= breaks[i]
    world$map$temperature_zone[these] <- names(breaks)[i]
  }

  world$map$temperature_zone <- factor(world$map$temperature_zone,
                            levels = rev(names(breaks)))

  report <- as.data.frame(rev(table(world$map$temperature_zone)))
  colnames(report) <- c("type", "n_tiles")
  report$pct <- round(report$n_tiles / sum(report$n_tiles) * 100, 2)

  # printing
  report <- capture.output(print(report, topn = nrow(report)))
  report <- sub("^\\d", " ", report)
  report <- paste0(report, collapse = "\n")
  message("Tile totals:\n", report)

  return(world)
}

classify_coasts <- function(world) {
  pb <- txtProgressBar(min = 0, max = sum(world$map$land), style = 3, width = 60)
  counter <- 0
  world$map$coastal <- FALSE
  # grab only land tiles, find if there's a
  # water tile next to them.
  world$map$coastal[world$map$land] <- 
    sapply(which(world$map$land), function(i) {
      counter <<- counter + 1
      setTxtProgressBar(pb, counter)
      link <- neighbour_ids(world, i)
      neighbours <- world$map[link, ]

      coastal <- any(grepl("ocean", neighbours$terrain))
      return(coastal)
    })
  close(pb)
  return(world)
}
