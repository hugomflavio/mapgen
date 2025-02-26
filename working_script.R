#!/usr/bin/Rscript
devtools::load_all()
library("ggplot2")

set.seed(1)
world <- gen_world(n_plates = c(10),
                   smooth = c(3),
                   stress = c(0),
                   height_range = c(20),
                   weight = 0.2,
                   map_x = 120, map_y = 120,
                   gravity_range = c(0.2, 1),
                   dist_method = "square",
                   noise = 0)

plot_stress(world)
# plot_plates(world) + guides(fill = "none")

topography <- data.frame(id = c("deep_water",
                              "water",
                              "low",
                              "med",
                              "high",
                              "peaks",
                              "ultra_peak"),
                       q = c(0,
                             0.50,
                             0.7,
                             0.8,
                             0.9,
                             0.98,
                             0.995),
                       fill = c("#3d74f5",
                                "#27cff5",
                                "#65f0a8",
                                "#33e622",
                                "#8ad184",
                                "#ced6ce",
                                "#fcfcfc"),
                       pct_hills = c(0,0,2,5,20,20,10),
                       pct_mountains = c(0,0,0,2,5,20,90))

world <- classify_heights(world, topography = topography)
world <- find_isolated(world)
world <- remove_isolated(world, type = "land")

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

world <- assign_vertical_terrain(world)
unique(world$map$terrain)

world$map$terrain[world$map$topography == "deep_water"] <- "deep_water"
world$map$terrain[world$map$topography == "water"] <- "water"

calc_slope <- function(world) {
  map_x <- max(world$map$x) 
  map_y <- max(world$map$y)
  pb <- txtProgressBar(min = 0, max = nrow(world$map), style = 3, width = 60)
  counter <- 0
  world$map$slope <- apply(world$map, 1, function(r) {
    counter <<- counter + 1
    setTxtProgressBar(pb, counter)
    # determine the range of nearby cells
    range_x <- wrapped_range(as.numeric(r["x"]), 1, map_x)
    range_y <- wrapped_range(as.numeric(r["y"]), 1, map_y)

    # pick the cells that match the x and y parameters
    rows_x <- world$map$x %in% range_x
    rows_y <- world$map$y %in% range_y
    neighbours <- world$map[rows_x & rows_y, ]

    slope <- max(neighbours$stress) - min(neighbours$stress)
    return(slope)
  })
  close(pb)
  return(world)
}

  p <- ggplot2::ggplot(data = world$map)
  p <- p + geom_tile(aes(x = x, y = y, fill = slope))
  p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
  p <- p + ggplot2::scale_y_continuous(expand = c(0, 0))
  p

plot_stress(world) + p
plot_stress(world) + plot_topography(world)

p_temp <- ggplot(data = world$map)
p_temp <- p_temp + geom_contour(aes(x = x, y = y, z = height))
p_temp <- p_temp + geom_contour(aes(x = x, y = y, z = as.numeric(land)), breaks = 1, linewidth = 1.5)
p_temp <- p_temp + geom_tile(aes(x = x, y = y, fill = temperature_zone), alpha = 0.7)
p_temp + scale_fill_brewer(palette = "RdYlBu")


world <- gen_temperature(world,
                        pole_locs = list(c(60.5, 0)),
                        pole_radius = 5,
                        pole_power = 1,
                        #hotspot_locs = gen_hotspot_locs(world, 10),
                        hotspot_radius = 2,
                        hotspot_power = 0.2,
                        noise = data.frame(
                            frequency = 0.5,
                            amplitude = 0.1
                        ),
                        min_land_effect = 0,
                        max_land_effect = 20,
                        min_water_effect = 3,
                        max_water_effect = 5)

plot_temperature(world)

world <- classify_temps(world, breaks = c("polar" = -999,
                                          "very_cold" = -35,
                                          "cold" = -15,
                                          "temperate" = 10,
                                          "warm" = 25,
                                          "very_warm" = 33))

p_temp <- ggplot(data = world$map)
p_temp <- p_temp + geom_contour(aes(x = x, y = y, z = height))
p_temp <- p_temp + geom_contour(aes(x = x, y = y, z = as.numeric(land)), breaks = 1, linewidth = 1.5)
p_temp <- p_temp + geom_tile(aes(x = x, y = y, fill = temperature_zone), alpha = 0.7)
p_temp + scale_fill_brewer(palette = "RdYlBu")

# p_stress1 <- ggplot(data = world1$map)
# p_stress1 <- p_stress1 + geom_tile(aes(x = x, y = y, fill = stress))
# p_stress1 <- p_stress1 + geom_point(data = world1$plates, aes(x = centre_x, y = centre_y))
# p_stress1 <- p_stress1 + geom_line(data = world1$vectors, aes(x = x, y = y, group = id))
# p_stress1 <- p_stress1 + guides(fill = "none")

# p_stress2 <- ggplot(data = world2$map)
# p_stress2 <- p_stress2 + geom_tile(aes(x = x, y = y, fill = stress))
# p_stress2 <- p_stress2 + geom_point(data = world2$plates, aes(x = centre_x, y = centre_y))
# p_stress2 <- p_stress2 + geom_line(data = world2$vectors, aes(x = x, y = y, group = id))
# p_stress2 <- p_stress2 + guides(fill = "none")

# (p_plates1 + p_plates2) / (p_stress1 + p_stress2)

