#!/usr/bin/Rscript
devtools::load_all()
library("ggplot2")

set.seed(1)
world <- gen_world(n_plates = c(20, 50),
                   smooth = c(1, 1),
                   stress = c(2, 5),
                   height_range = c(20, 20),
                   weight = 1,
                   map_x = 120, map_y = 120,
                   gravity_range = c(1, 1),
                   dist_method = "square",
                   noise = 50)

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
# world <- find_isolated(world)
# world <- remove_isolated(world, type = "land")

world <- assign_vertical_terrain(world)
unique(world$map$terrain)

world$map$terrain[world$map$topography == "deep_water"] <- "deep_water"
world$map$terrain[world$map$topography == "water"] <- "water"

world <- calc_slope(world)
p <- ggplot2::ggplot(data = world$map)
p <- p + geom_tile(aes(x = x, y = y, fill = slope))
p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
p <- p + ggplot2::scale_y_continuous(expand = c(0, 0))
p

plot_stress(world) + p
plot_stress(world) + plot_topography(world)

world <- gen_temperature(world,
                        pole_locs = list(c(60.5, 0)),
                        pole_radius = 5,
                        pole_power = 1,
                        #hotspot_locs = gen_hotspot_locs(world, 10),
                        hotspot_radius = 2,
                        hotspot_power = 0.2,
                        noise = data.frame(
                            frequency = 0.1,
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

