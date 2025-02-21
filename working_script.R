#!/usr/bin/Rscript
devtools::load_all()
library("ggplot2")

set.seed(1)

world1 <- gen_plates(50, map_x = 120, map_y = 120)
world2 <- gen_plates(100, map_x = 120, map_y = 120)

p_plates1 <- ggplot(data = world1$map)
p_plates1 <- p_plates1 + geom_tile(aes(x = x, y = y, fill = as.factor(plate)))
p_plates1 <- p_plates1 + geom_point(data = world1$plates, aes(x = centre_x, y = centre_y))
p_plates1 <- p_plates1 + geom_line(data = world1$vectors, aes(x = x, y = y, group = id))
p_plates1 <- p_plates1 + guides(fill = "none")

p_plates2 <- ggplot(data = world2$map)
p_plates2 <- p_plates2 + geom_tile(aes(x = x, y = y, fill = as.factor(plate)))
p_plates2 <- p_plates2 + geom_point(data = world2$plates, aes(x = centre_x, y = centre_y))
p_plates2 <- p_plates2 + geom_line(data = world2$vectors, aes(x = x, y = y, group = id))
p_plates2 <- p_plates2 + guides(fill = "none")

world1 <- calc_stress(world1, spread = 5)
world1 <- scale_stress(world1)
world2 <- calc_stress(world2, spread = 5)
world2 <- scale_stress(world2)

p_stress1 <- ggplot(data = world1$map)
p_stress1 <- p_stress1 + geom_tile(aes(x = x, y = y, fill = stress))
p_stress1 <- p_stress1 + geom_point(data = world1$plates, aes(x = centre_x, y = centre_y))
p_stress1 <- p_stress1 + geom_line(data = world1$vectors, aes(x = x, y = y, group = id))
p_stress1 <- p_stress1 + guides(fill = "none")

p_stress2 <- ggplot(data = world2$map)
p_stress2 <- p_stress2 + geom_tile(aes(x = x, y = y, fill = stress))
p_stress2 <- p_stress2 + geom_point(data = world2$plates, aes(x = centre_x, y = centre_y))
p_stress2 <- p_stress2 + geom_line(data = world2$vectors, aes(x = x, y = y, group = id))
p_stress2 <- p_stress2 + guides(fill = "none")

# (p_plates1 + p_plates2) / (p_stress1 + p_stress2)

world_final <- world1
world_final$map$stress <- world1$map$stress + world2$map$stress
# world_final$map$stress <- world1$map$stress + round(world2$map$stress / 2, 0)

world_final <- scale_stress(world_final)

world_final$map$stress <- world_final$map$stress + rnorm(nrow(world_final$map), 0, 50)


p_stress_final <- ggplot(data = world_final$map)
p_stress_final <- p_stress_final + geom_tile(aes(x = x, y = y, fill = stress))
p_stress_final <- p_stress_final + geom_point(data = world_final$plates, aes(x = centre_x, y = centre_y))
# p_stress_final <- p_stress_final + geom_line(data = world_final$vectors, aes(x = x, y = y, group = id))
p_stress_final <- p_stress_final + geom_point(data = world2$plates, aes(x = centre_x, y = centre_y), colour = "red")
# p_stress_final <- p_stress_final + geom_line(data = world2$vectors, aes(x = x, y = y, group = id), colour = "red")
# p_stress_final


deep_water <- quantile(world_final$map$stress, 0.55) #0.35
water <- quantile(world_final$map$stress, 0.7) #0.5
low <- quantile(world_final$map$stress, 0.8)
med <- quantile(world_final$map$stress, 0.90)
high <- quantile(world_final$map$stress, 0.98)

world_final$map$topography <- "deeps"
world_final$map$topography[world_final$map$stress > deep_water] <- "shallows"
world_final$map$topography[world_final$map$stress > water] <- "low"
world_final$map$topography[world_final$map$stress > low] <- "med"
world_final$map$topography[world_final$map$stress > med] <- "high"
world_final$map$topography[world_final$map$stress > high] <- "peaks"

world_final$map$topography <- factor(world_final$map$topography,
                               levels = c("peaks",
                                          "high",
                                          "med",
                                          "low",
                                          "shallows",
                                          "deeps"))

map_colours <- c("deeps" = "#3d74f5",
                 "shallows" = "#27cff5",
                 "low" = "#65f0a8",
                 "med" = "#33e622",
                 "high" = "#8ad184",
                 "peaks" = "#ced6ce")
# dev.new()

p_world <- ggplot(data = world_final$map)
p_world <- p_world + geom_tile(aes(x = x, y = y, fill = topography))
# p <- p + geom_point(data = world_final$plates, aes(x = centre_x, y = centre_y))
# p <- p + geom_line(data = world_final$vectors, aes(x = x, y = y, group = id))
# p <- p + geom_point(data = world2$plates, aes(x = centre_x, y = centre_y), colour = "grey40")
# p <- p + geom_line(data = world2$vectors, aes(x = x, y = y, group = id), colour = "grey40", linetype = "dashed")
p_world <- p_world + scale_fill_manual(values = map_colours)


p_plates1 + p_plates2 + p_stress1 + p_stress2 + p_stress_final + plot_layout(design = "ABEE\nCDEE")

p_stress_final + p_world
