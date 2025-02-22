#!/usr/bin/Rscript
devtools::load_all()
library("ggplot2")

set.seed(1)
world <- gen_world(n_plates = c(10, 5, 50), spread = c(1, 20, 5),
                   weight = 0.5,
                   map_x = 120, map_y = 120,
                   gravity_range = c(0.2, 1),
                   height_range = c(-10, 10),
                   dist_method = "square")

# plot_plates(world) + guides(fill = "none")

world <- classify_heights(world, breaks = c("deep_water" = 0,
                                            "water" = 0.50,
                                            "low" = 0.7,
                                            "med" = 0.8,
                                            "high" = 0.9,
                                            "peaks" = 0.98))

world <- find_isolated(world)
world <- remove_isolated(world, type = "land")

plot_topography(world, fill = c("deep_water" = "#3d74f5",
                                "water" = "#27cff5",
                                "low" = "#65f0a8",
                                "med" = "#33e622",
                                "high" = "#8ad184",
                                "peaks" = "#ced6ce"))

# plot_contours(world)

# plot_land(world)


plot_land(world)

world <- gen_temperature(world,
                        pole_locs = list(c(60.5, 0)),
                        pole_radius = 5,
                        pole_power = 2,
                        hotspot_locs = gen_hotspot_locs(world, 10),
                        hotspot_radius = 2,
                        min_land_effect = 0,
                        max_land_effect = 30,
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



