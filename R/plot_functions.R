plot_plates <- function(world, points = TRUE, gravity = TRUE, vectors = TRUE) {
  p <- ggplot2::ggplot(data = world$map)
  p <- p + ggplot2::geom_tile(ggplot2::aes(x = x, y = y,
                                           fill = as.factor(plate)),
                              width = 1, height = 1)
  if (points) {
    p <- p + ggplot2::geom_point(data = world$plates,
                                 ggplot2::aes(x = centre_x, y = centre_y))
  }
  if (gravity) {
    p <- p + ggplot2::geom_text(data = world$plates,
                                ggplot2::aes(x = centre_x, y = centre_y,
                                             label = round(gravity, 3)),
                                position = ggplot2::position_nudge(x = 0,
                                                                   y = 2))
  }
  if (vectors) {
    p <- p + ggplot2::geom_line(data = world$vectors,
                                ggplot2::aes(x = x, y = y, group = id))
  }
  return(p)
}

plot_stress <- function(world) {
  p <- ggplot2::ggplot(data = world$map)
  p <- p + ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = stress),
                              width = 1, height = 1)
  p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
  p <- p + ggplot2::scale_y_continuous(expand = c(0, 0))
  return(p)
}

plot_topography <- function(world) {

  if (is.null(world$map$topography)) {
    stop("This world has no topography yet.", call. = FALSE)
  }
  fill <- world$topography$fill
  names(fill) <- world$topography$id
  
  p <- ggplot2::ggplot(data = world$map)
  p <- p + ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = topography),
                              width = 1, height = 1)
  p <- p + ggplot2::scale_fill_manual(values = fill)
  return(p)
}

plot_contours <- function(world, colour = "brown") {
  if (is.null(world$map$topography)) {
    stop("This world has no topography yet.", call. = FALSE)
  }

  world$map$aux <- world$map$topography
  water <- grepl("water", levels(world$map$aux))
  levels(world$map$aux)[water] <- "water"
  world$map$aux <- as.numeric(world$map$aux)
  breaks <- 1:length(unique(world$map$aux))
  
  p <- ggplot2::ggplot(data = world$map)
  p <- p + ggplot2::geom_contour(ggplot2::aes(x = x, y = y,
                                              z = aux),
                                 breaks = breaks,
                                 colour = colour)
  p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
  p <- p + ggplot2::scale_y_continuous(expand = c(0, 0))
  return(p)
}

plot_land <- function(world, fill = c("white", "grey")) {
  p <- ggplot2::ggplot(data = world$map)
  p <- p + ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = land),
                              width = 1, height = 1)
  p <- p + ggplot2::scale_fill_manual(values = fill)
  p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
  p <- p + ggplot2::scale_y_continuous(expand = c(0, 0))
  return(p)
}

plot_temperature <- function(world) {
  p <- ggplot(data = world$map)
  p <- p + geom_contour(aes(x = x, y = y, z = height))
  p <- p + geom_tile(aes(x = x, y = y, fill = temperature_real), alpha = 0.7,
                     width = 1, height = 1)
  p <- p + scale_fill_gradient2(low = "blue", mid = "white", high = "red")
  return(p)
}


plot_terrain <- function(world) {
  p <- ggplot2::ggplot(data = world$map)
  p <- p + ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = terrain),
                              width = 1, height = 1)
  p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
  p <- p + ggplot2::scale_y_continuous(expand = c(0, 0))
  return(p)
}

plot_ids <- function(world, ids, label) {
  if (is.null(world$map$topography)) {
    stop("This world has no topography yet.", call. = FALSE)
  }
  fill <- world$topography$fill
  names(fill) <- world$topography$id

  range_x <- range(world$map$x[ids])
  range_y <- range(world$map$y[ids])
  link_x <- world$map$x >= range_x[1]-1 & world$map$x <= range_x[2]+1
  link_y <- world$map$y >= range_y[1]-1 & world$map$y <= range_y[2]+1
  map <- world$map[link_x & link_y, ]

  p <- ggplot2::ggplot(data = map)
  p <- p + ggplot2::aes(x = x, y = y)
  p <- p + ggplot2::geom_tile(ggplot2::aes(fill = topography),
                              width = 1, height = 1)
  p <- p + ggplot2::scale_fill_manual(values = fill)

  p <- p + ggplot2::geom_tile(data  = world$map[ids, ],
                              linewidth = 1.5,
                              fill = "red",
                              col = "red",
                              alpha = 0.2,
                              width = 1, height = 1)
  if (!missing(label)) {
    p <- p + geom_text(aes(x = x, y = y, label = {{label}}))
  }
  return(p)
}
