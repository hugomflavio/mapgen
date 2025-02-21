calc_stress <- function(world, spread = 5) {
  map_x <- max(world$map$x) 
  map_y <- max(world$map$y)
  pb <- txtProgressBar(min = 0, max = nrow(world$map), style = 3, width = 60)
  counter <- 0
  world$map$stress <- apply(world$map, 1, function(r) {
    counter <<- counter + 1
    setTxtProgressBar(pb, counter)
    # determine the range of nearby cells
    range_x <- wrapped_range(r["x"], spread, map_x)
    range_y <- wrapped_range(r["y"], spread, map_y)

    # pick the cells that match the x and y parameters
    rows_x <- world$map$x %in% range_x
    rows_y <- world$map$y %in% range_y
    neighbours <- rows_x & rows_y
    # if all neighbours belong to same plate, skip
    if (length(unique(world$map$plate[neighbours])) == 1) {
      return(0)
    # otherwise...
    } else {
      not_r_plate <- neighbours & world$map$plate != r["plate"]
  
      rel_x <- wrapped_distance(r["x"], world$map$x[not_r_plate], map_x)
      rel_y <- wrapped_distance(r["y"], world$map$y[not_r_plate], map_y)

      rel_force_x <- world$map$force_x[not_r_plate] - r["force_x"]
      rel_force_y <- world$map$force_y[not_r_plate] - r["force_y"]
      force <- sqrt(rel_force_x^2 + rel_force_y^2)
      stress <- (rel_x * rel_force_x / force) + (rel_y * rel_force_y / force)
      return(-sum(stress))
    }
  })
  close(pb)
  return(world)
}

scale_stress <- function(world) {
  world$map$stress <- world$map$stress - mean(world$map$stress)
  world$map$stress <- world$map$stress / sd(world$map$stress)
  world$map$stress <- world$map$stress * 333
  return(world)
}
