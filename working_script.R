#!/usr/bin/Rscript
devtools::load_all()
library("ggplot2")

set.seed(1)

world <- gen_world(n_plates = c(10, 50), spread = c(10, 5),
                   map_x = 120, map_y = 120)

world <- classify_heights(world, breaks = c("deep_water" = 0,
                                            "water" = 0.55,
                                            "low" = 0.7,
                                            "med" = 0.8,
                                            "high" = 0.9,
                                            "peaks" = 0.98))

p_stress_final <- ggplot(data = world$map)
p_stress_final <- p_stress_final + geom_tile(aes(x = x, y = y, fill = stress))

map_colours <- c("deep_water" = "#3d74f5",
                 "water" = "#27cff5",
                 "low" = "#65f0a8",
                 "med" = "#33e622",
                 "high" = "#8ad184",
                 "peaks" = "#ced6ce")

p_world <- ggplot(data = world$map)
p_world <- p_world + geom_tile(aes(x = x, y = y, fill = topography))
p_world <- p_world + scale_fill_manual(values = map_colours)

p_stress_final + p_world















# p_plates1 <- ggplot(data = world1$map)
# p_plates1 <- p_plates1 + geom_tile(aes(x = x, y = y, fill = as.factor(plate)))
# p_plates1 <- p_plates1 + geom_point(data = world1$plates, aes(x = centre_x, y = centre_y))
# p_plates1 <- p_plates1 + geom_line(data = world1$vectors, aes(x = x, y = y, group = id))
# p_plates1 <- p_plates1 + guides(fill = "none")

# p_plates2 <- ggplot(data = world2$map)
# p_plates2 <- p_plates2 + geom_tile(aes(x = x, y = y, fill = as.factor(plate)))
# p_plates2 <- p_plates2 + geom_point(data = world2$plates, aes(x = centre_x, y = centre_y))
# p_plates2 <- p_plates2 + geom_line(data = world2$vectors, aes(x = x, y = y, group = id))
# p_plates2 <- p_plates2 + guides(fill = "none")


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



