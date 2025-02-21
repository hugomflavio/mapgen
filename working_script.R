#!/usr/bin/Rscript
devtools::load_all()
library("ggplot2")

set.seed(1)
world <- gen_world(n_plates = c(10, 50), spread = c(20, 5),
                   weight = 0.2,
                   map_x = 120, map_y = 120,
                   gravity_range = c(0.4, 1),
                   dist_method = "square")

world <- classify_heights(world, breaks = c("deep_water" = 0,
                                            "water" = 0.55,
                                            "low" = 0.7,
                                            "med" = 0.8,
                                            "high" = 0.9,
                                            "peaks" = 0.98))


# p_plates <- ggplot(data = world$map)
# p_plates <- p_plates + geom_tile(aes(x = x, y = y, fill = as.factor(plate)))
# p_plates <- p_plates + geom_point(data = world$plates, aes(x = centre_x, y = centre_y))
# p_plates <- p_plates + geom_text(data = world$plates, aes(x = centre_x, y = centre_y, label = round(gravity, 3)),
#                                  position = position_nudge(x = 0, y = 2))
# p_plates <- p_plates + geom_line(data = world$vectors, aes(x = x, y = y, group = id))
# p_plates <- p_plates + guides(fill = "none")
# p_plates

# p_stress_final <- ggplot(data = world$map)
# p_stress_final <- p_stress_final + geom_tile(aes(x = x, y = y, fill = stress))
# p_stress_final


map_colours <- c("deep_water" = "#3d74f5",
                 "water" = "#27cff5",
                 "low" = "#65f0a8",
                 "med" = "#33e622",
                 "high" = "#8ad184",
                 "peaks" = "#ced6ce")

p_world <- ggplot(data = world$map)
p_world <- p_world + geom_tile(aes(x = x, y = y, fill = topography))
p_world <- p_world + scale_fill_manual(values = map_colours)
p_world


p_stress_final + p_world

world$map$land <- factor(world$map$topography, levels = rev(levels(world$map$topography)))
world$map$land <- as.numeric(world$map$land) - 1
world$map$land[world$map$land < 0] <- 0

world$map$land <- 0
world$map$land[!(world$map$topography %in% c("deep_water", "water"))] <- 1


p_world <- ggplot(data = world$map)
p_world <- p_world + geom_tile(aes(x = x, y = y, fill = topography))
p_world <- p_world + geom_contour(aes(x = x, y = y, z = stress))
p_world <- p_world + scale_fill_manual(values = map_colours)
p_world







# Function to calculate distance
wrap_distance <- function(a, b) {
  # Convert inputs to numeric vectors and apply modulo 1
  a <- a %% 1
  b <- b %% 1
  
  # Calculate the difference (delta)
  delta <- a - b
  
  # Adjust values where delta > 0.5
  delta[delta > 0.5] <- 1 - delta[delta > 0.5]
  
  # Return the Euclidean distance (norm)
  return(sqrt(sum(delta^2)))
}

# Main temperature function
temperature <- function(x, y, r, pole_loc = c(0, 0), hotspot_locs = list(c(1, 1))) {
  
  # Calculate the distance to the pole
  distance_to_pole <- wrap_distance(c(x, y), pole_loc)
  
  # Calculate distances to all hotspot locations
  distances_to_hotspots <- sapply(hotspot_locs, function(hotspot) {
    wrap_distance(c(x, y), hotspot)
  })
  
  # Find the minimum distance to the hotspot
  distance_to_hotspot <- min(distances_to_hotspots)
  
  # Calculate the hotspot modifier
  hotspot_modifier <- (distance_to_hotspot - r) / (distance_to_hotspot + r * 2)
  
  # Calculate the temperature based on distance to the pole
  temperature <- (distance_to_pole - r) / (distance_to_pole + r)
  
  # Adjust temperature by the hotspot modifier
  temperature <- temperature - hotspot_modifier / 5
  
  return(temperature)
}

# Main plotting script
library(ggplot2)
library(gridExtra)

# Set up grid
x <- seq(0, 1, length.out = 120)
y <- seq(0, 1, length.out = 120)
grid <- expand.grid(x = x, y = y)

# r value
r <- 0.05

# Generate random hotspot locations
num_hotspots <- 1 + rpois(1, 3)
hotspot_x <- rnorm(num_hotspots, mean = 0, sd = 0.4)
hotspot_y <- rnorm(num_hotspots, mean = 0.5, sd = 0.15)
hotspot_locs <- lapply(1:num_hotspots, function(i) c(hotspot_x[i], hotspot_y[i]))

# Apply temperature function
T <- matrix(NA, nrow = length(x), ncol = length(y))
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    T[i, j] <- temperature(x[i], y[j], r, c(0.5, 0), hotspot_locs)
  }
}

# Plotting the contour

aux <- as.data.frame(T)
aux$y <- 1:120

output <- reshape2::melt(aux, id.vars = "y")
output$value[output$value < 0] <- 0
output$value <- output$value * 100
output$value <- output$value - 50

world$map$temperature <- output$value

p_temp <- ggplot(data = world$map)
p_temp <- p_temp + geom_tile(aes(x = x, y = y, fill = temperature))
p_temp + scale_fill_gradient2(low = "blue", mid = "white", high = "red")
p_temp

aux <- world$map$topography == "deep_water"
world$map$temperature[aux] <- world$map$temperature[aux] - 5

aux <- world$map$topography == "water"
world$map$temperature[aux] <- world$map$temperature[aux] - 3

aux <- world$map$topography == "high"
world$map$temperature[aux] <- world$map$temperature[aux] - 5

aux <- world$map$topography == "peaks"
world$map$temperature[aux] <- world$map$temperature[aux] - 15

world <- classify_temps(world, breaks = c("polar" = -999,
                                          "very_cold" = -40,
                                          "cold" = -5,
                                          "temperate" = 10,
                                          "warm" = 20,
                                          "very_warm" = 30))

round(table(world$map$biome)/nrow(world$map)*100, 2)

p_temp <- ggplot(data = world$map)
p_temp <- p_temp + geom_contour(aes(x = x, y = y, z = land), breaks = 1, linewidth = 1.5)
p_temp <- p_temp + geom_contour(aes(x = x, y = y, z = land), breaks = 1:5)
p_temp <- p_temp + geom_tile(aes(x = x, y = y, fill = biome), alpha = 0.7)
p_temp + scale_fill_brewer(palette = "RdYlBu")

p_world <- ggplot(data = world$map)
p_world


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



